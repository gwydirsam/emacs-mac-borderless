/* Functions for GUI implemented with Cocoa AppKit on the Mac OS.
   Copyright (C) 2008, 2009 YAMAMOTO Mitsuharu

This file is part of GNU Emacs Mac port.

GNU Emacs Mac port is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

GNU Emacs Mac port is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Emacs Mac port.  If not, see <http://www.gnu.org/licenses/>.  */

#include <config.h>
#include "lisp.h"
#include "blockinput.h"

#include "macterm.h"

#include "charset.h"
#include "character.h"
#include "frame.h"
#include "dispextern.h"
#include "fontset.h"
#include "termhooks.h"
#include "buffer.h"
#include "window.h"
#include "keyboard.h"
#include "intervals.h"
#include "keymap.h"

#if MAC_OS_X_VERSION_MIN_REQUIRED < 1050
#include "macfont.h"
#endif

#import "macappkit.h"
#if MAC_OS_X_VERSION_MAX_ALLOWED >= 1050
#import <objc/runtime.h>
#endif

/************************************************************************
			       General
 ************************************************************************/

extern Lisp_Object Qdictionary;

enum {
  ANY_MOUSE_EVENT_MASK = (NSLeftMouseDownMask | NSLeftMouseUpMask
			  | NSRightMouseDownMask | NSRightMouseUpMask
			  | NSMouseMovedMask
			  | NSLeftMouseDraggedMask | NSRightMouseDraggedMask
			  | NSMouseEnteredMask | NSMouseExitedMask
			  | NSScrollWheelMask
			  | NSOtherMouseDownMask | NSOtherMouseUpMask
			  | NSOtherMouseDraggedMask),
  ANY_MOUSE_DOWN_EVENT_MASK = (NSLeftMouseDownMask | NSRightMouseDownMask
			       | NSOtherMouseDownMask)
};

enum {
  ANY_KEY_MODIFIER_FLAGS_MASK = (NSAlphaShiftKeyMask | NSShiftKeyMask
				 | NSControlKeyMask | NSAlternateKeyMask
				 | NSCommandKeyMask | NSNumericPadKeyMask
				 | NSHelpKeyMask | NSFunctionKeyMask)
};

#define CFOBJECT_TO_LISP_FLAGS_FOR_EVENT			\
  (CFOBJECT_TO_LISP_WITH_TAG					\
   | CFOBJECT_TO_LISP_DONT_DECODE_STRING			\
   | CFOBJECT_TO_LISP_DONT_DECODE_DICTIONARY_KEY)

#if MAC_OS_X_VERSION_MAX_ALLOWED < 1050
INLINE NSRect
NSRectFromCGRect (cgrect)
     CGRect cgrect;
{
  union _ {NSRect ns; CGRect cg;};

  return ((union _ *) &cgrect)->ns;
}

INLINE CGRect
NSRectToCGRect (nsrect)
     NSRect nsrect;
{
  union _ {NSRect ns; CGRect cg;};

  return ((union _ *) &nsrect)->cg;
}
#endif

@implementation NSData (Emacs)

/* Return a unibyte Lisp string.  */

- (Lisp_Object)lispString
{
  return cfdata_to_lisp ((CFDataRef) self);
}

@end				// NSData (Emacs)

@implementation NSString (Emacs)

/* Return a string created from the Lisp string.  May cause GC.  */

+ (id)stringWithLispString:(Lisp_Object)lispString
{
  id string = (NSString *) cfstring_create_with_string (lispString);

  return [string autorelease];
}

/* Like -[NSString stringWithUTF8String:], but fall back on Mac-Roman
   if BYTES cannot be interpreted as UTF-8 bytes and FLAG is YES. */

+ (id)stringWithUTF8String:(const char *)bytes fallback:(BOOL)flag
{
  id string = [[self class] stringWithUTF8String:bytes];

  if (string == nil && flag)
    {
      string = ((NSString *)
		CFStringCreateWithCString (NULL, bytes,
					   kCFStringEncodingMacRoman));
      [string autorelease];
    }

  return string;
}

/* Return a multibyte Lisp string.  May cause GC.  */

- (Lisp_Object)lispString
{
  return cfstring_to_lisp ((CFStringRef) self);
}

/* Return a unibyte Lisp string in UTF 8.  */

- (Lisp_Object)UTF8LispString
{
  return cfstring_to_lisp_nodecode ((CFStringRef) self);
}

/* Return a unibyte Lisp string in UTF 16 (native byte order, no BOM).  */

- (Lisp_Object)UTF16LispString
{
  return cfstring_to_lisp_utf_16 ((CFStringRef) self);
}

@end				// NSString (Emacs)

@implementation NSFont (Emacs)

/* Return an NSFont object for the specified FACE.  */

+ (NSFont *)fontWithFace:(struct face *)face
{
  if (face == NULL || face->font == NULL)
    return nil;

  return macfont_get_nsctfont (face->font);
}

@end				// NSFont (Emacs)

@implementation NSEvent (Emacs)

- (NSEvent *)mouseEventByChangingType:(NSEventType)type
			  andLocation:(NSPoint)location
{
  return [NSEvent mouseEventWithType:type location:location
		  modifierFlags:[self modifierFlags] timestamp:[self timestamp]
		  windowNumber:[self windowNumber] context:[self context]
		  eventNumber:[self eventNumber] clickCount:[self clickCount]
		  pressure:[self pressure]];
}

@end				// NSEvent (Emacs)

@implementation NSAttributedString (Emacs)

/* Return a unibyte Lisp string with text properties, in UTF 16
   (native byte order, no BOM).  */

- (Lisp_Object)UTF16LispString
{
  Lisp_Object result = [[self string] UTF16LispString];
  NSUInteger length = [self length];
  NSRange range = NSMakeRange (0, 0);

  while (NSMaxRange (range) < length)
    {
      Lisp_Object attrs = Qnil;
      NSDictionary *attributes = [self attributesAtIndex:NSMaxRange (range)
				       effectiveRange:&range];

      if (attributes)
	attrs = cfobject_to_lisp ((CFTypeRef) attributes,
				  CFOBJECT_TO_LISP_FLAGS_FOR_EVENT, -1);
      if (CONSP (attrs) && EQ (XCAR (attrs), Qdictionary))
	{
	  Lisp_Object props = Qnil, start, end;

	  for (attrs = XCDR (attrs); CONSP (attrs); attrs = XCDR (attrs))
	    props = Fcons (Fintern (XCAR (XCAR (attrs)), Qnil),
			   Fcons (XCDR (XCAR (attrs)), props));

	  XSETINT (start, range.location * sizeof (unichar));
	  XSETINT (end, NSMaxRange (range) * sizeof (unichar));
	  Fadd_text_properties (start, end, props, result);
	}
    }

  return result;
}

@end				// NSAttributedString (Emacs)

@implementation NSImage (Emacs)

/* Create an image object from a Quartz 2D image.  */

+ (id)imageWithCGImage:(CGImageRef)cgImage
{
  NSRect rect = NSMakeRect (0, 0, CGImageGetWidth (cgImage),
			    CGImageGetHeight (cgImage));
  id image = [[[self class] alloc] initWithSize:rect.size];
  CGContextRef context;

  [image lockFocus];
  context = [[NSGraphicsContext currentContext] graphicsPort];
  CGContextDrawImage (context, NSRectToCGRect (rect), cgImage);
  [image unlockFocus];

  return [image autorelease];
}

@end				// NSImage (Emacs)

@implementation NSApplication (Emacs)

- (void)postDummyEvent
{
  NSEvent *event = [NSEvent otherEventWithType:NSApplicationDefined
			    location:NSZeroPoint modifierFlags:0
			    timestamp:0 windowNumber:0 context:nil
			    subtype:0 data1:0 data2:0];

  [self postEvent:event atStart:YES];
}

- (void)stopAfterInvocation:(id)anArgument
{
  [anArgument invoke];
  [self stop:nil];
  [self postDummyEvent];
}

/* Temporarily run the main event loop during the given
   invocation.  */

- (void)runTemporarilyWithInvocation:(NSInvocation *)invocation
{
  [[NSRunLoop currentRunLoop]
    performSelector:@selector(stopAfterInvocation:)
    target:self argument:invocation order:0
    modes:[NSArray arrayWithObject:NSDefaultRunLoopMode]];
  [self run];
}

@end				// NSApplication (Emacs)

@implementation EmacsPosingWindow

#if MAC_OS_X_VERSION_MAX_ALLOWED >= 1050
/* Variables to save implementations of the original -[NSWindow close]
   and -[NSWindow orderOut:].  */
static IMP impClose, impOrderOut;
#endif

+ (void)setup
{
#if MAC_OS_X_VERSION_MAX_ALLOWED >= 1050
#if MAC_OS_X_VERSION_MIN_REQUIRED < 1050 && MAC_OS_X_VERSION_MIN_REQUIRED >= 1020
  if (method_getImplementation != NULL)
#endif
    {
      Method methodClose =
	class_getInstanceMethod ([NSWindow class], @selector(close));
      Method methodOrderOut =
	class_getInstanceMethod ([NSWindow class], @selector(orderOut:));
      Method methodCloseNew =
	class_getInstanceMethod ([self class], @selector(close));
      Method methodOrderOutNew =
	class_getInstanceMethod ([self class], @selector(orderOut:));
      IMP impCloseNew = method_getImplementation (methodCloseNew);
      IMP impOrderOutNew = method_getImplementation (methodOrderOutNew);

      impClose = method_setImplementation (methodClose, impCloseNew);
      impOrderOut = method_setImplementation (methodOrderOut, impOrderOutNew);
    }
#if MAC_OS_X_VERSION_MIN_REQUIRED < 1050 && MAC_OS_X_VERSION_MIN_REQUIRED >= 1020
  else				/* method_getImplementation == NULL */
#endif
#endif	/* MAC_OS_X_VERSION_MAX_ALLOWED >= 1050  */
#if MAC_OS_X_VERSION_MAX_ALLOWED < 1050 || (MAC_OS_X_VERSION_MIN_REQUIRED < 1050 && MAC_OS_X_VERSION_MIN_REQUIRED >= 1020)
    {
      [self poseAsClass:[NSWindow class]];
    }
#endif
}

/* Close the receiver with running the main event loop if not.  Just
   closing the window outside the application loop does not activate
   the next window.  */

- (void)close
{
  if ([NSApp isRunning])
    {
#if MAC_OS_X_VERSION_MAX_ALLOWED >= 1050
#if MAC_OS_X_VERSION_MIN_REQUIRED < 1050 && MAC_OS_X_VERSION_MIN_REQUIRED >= 1020
      if (method_getImplementation != NULL)
#endif
	{
	  (*impClose) (self, _cmd);
	}
#if MAC_OS_X_VERSION_MIN_REQUIRED < 1050 && MAC_OS_X_VERSION_MIN_REQUIRED >= 1020
      else			/* method_getImplementation == NULL */
#endif
#endif	/* MAC_OS_X_VERSION_MAX_ALLOWED >= 1050  */
#if MAC_OS_X_VERSION_MAX_ALLOWED < 1050 || (MAC_OS_X_VERSION_MIN_REQUIRED < 1050 && MAC_OS_X_VERSION_MIN_REQUIRED >= 1020)
	{
	  [super close];
	}
#endif
    }
  else
    {
      NSMethodSignature *signature = [self methodSignatureForSelector:_cmd];
      NSInvocation *invocation =
	[NSInvocation invocationWithMethodSignature:signature];

      [invocation setTarget:self];
      [invocation setSelector:_cmd];

      [NSApp runTemporarilyWithInvocation:invocation];
    }
}

/* Hide the receiver with running the main event loop if not.  Just
   hiding the window outside the application loop does not activate
   the next window.  */

- (void)orderOut:(id)sender
{
  if ([NSApp isRunning])
    {
#if MAC_OS_X_VERSION_MAX_ALLOWED >= 1050
#if MAC_OS_X_VERSION_MIN_REQUIRED < 1050 && MAC_OS_X_VERSION_MIN_REQUIRED >= 1020
      if (method_getImplementation != NULL)
#endif
	{
	  (*impOrderOut) (self, _cmd, sender);
	}
#if MAC_OS_X_VERSION_MIN_REQUIRED < 1050 && MAC_OS_X_VERSION_MIN_REQUIRED >= 1020
      else			/* method_getImplementation == NULL */
#endif
#endif	/* MAC_OS_X_VERSION_MAX_ALLOWED >= 1050  */
#if MAC_OS_X_VERSION_MAX_ALLOWED < 1050 || (MAC_OS_X_VERSION_MIN_REQUIRED < 1050 && MAC_OS_X_VERSION_MIN_REQUIRED >= 1020)
	{
	  [super orderOut:sender];
	}
#endif
    }
  else
    {
      NSMethodSignature *signature = [self methodSignatureForSelector:_cmd];
      NSInvocation *invocation =
	[NSInvocation invocationWithMethodSignature:signature];

      [invocation setTarget:self];
      [invocation setSelector:_cmd];
      [invocation setArgument:&sender atIndex:2];

      [NSApp runTemporarilyWithInvocation:invocation];
    }
}

@end				// EmacsPosingWindow

static EventRef current_text_input_event;

static pascal OSStatus
mac_handle_text_input_event (next_handler, event, data)
     EventHandlerCallRef next_handler;
     EventRef event;
     void *data;
{
  OSStatus result;

  switch (GetEventKind (event))
    {
    case kEventTextInputUpdateActiveInputArea:
    case kEventTextInputUnicodeForKeyEvent:
      {
	EventRef saved_text_input_event = current_text_input_event;

	current_text_input_event = RetainEvent (event);
	result = CallNextEventHandler (next_handler, event);
	current_text_input_event = saved_text_input_event;
	ReleaseEvent (event);
      }
      break;

    default:
      abort ();
    }

  return result;
}

#if MAC_OS_X_VERSION_MAX_ALLOWED >= 1030
static BOOL handling_document_access_lock_document_p = NO;

static pascal OSStatus
mac_handle_document_access_event (next_handler, event, data)
     EventHandlerCallRef next_handler;
     EventRef event;
     void *data;
{
  OSStatus result;

  switch (GetEventKind (event))
    {
    case kEventTSMDocumentAccessLockDocument:
    case kEventTSMDocumentAccessUnlockDocument:
      handling_document_access_lock_document_p = YES;
      result = CallNextEventHandler (next_handler, event);
      handling_document_access_lock_document_p = NO;
      break;

    default:
      abort ();
    }

  return result;
}
#endif

static OSStatus
install_dispatch_handler ()
{
  OSStatus err = noErr;

  if (err == noErr)
    {
      static const EventTypeSpec specs[] =
	{{kEventClassTextInput, kEventTextInputUpdateActiveInputArea},
	 {kEventClassTextInput, kEventTextInputUnicodeForKeyEvent}};

      err = InstallEventHandler (GetEventDispatcherTarget (),
				 mac_handle_text_input_event,
				 GetEventTypeCount (specs), specs, NULL, NULL);
    }

#if MAC_OS_X_VERSION_MAX_ALLOWED >= 1030
  if (err == noErr)
    {
      static const EventTypeSpec specs[] =
	{{kEventClassTSMDocumentAccess, kEventTSMDocumentAccessLockDocument},
	 {kEventClassTSMDocumentAccess, kEventTSMDocumentAccessUnlockDocument}};

      err = InstallEventHandler (GetEventDispatcherTarget (),
				 mac_handle_document_access_event,
				 GetEventTypeCount (specs), specs, NULL, NULL);
    }
#endif

  return err;
}

/* Autorelease pool.  */

void *
mac_alloc_autorelease_pool ()
{
  NSAutoreleasePool *pool;

  if (noninteractive)
    return NULL;

  BLOCK_INPUT;
  pool = [[NSAutoreleasePool alloc] init];
  UNBLOCK_INPUT;

  return pool;
}

void
mac_release_autorelease_pool (pool)
     void *pool;
{
  if (noninteractive)
    return;

  BLOCK_INPUT;
  [(NSAutoreleasePool *)pool release];
  UNBLOCK_INPUT;
}

void
mac_alert_sound_play ()
{
  NSBeep ();
}

double
mac_appkit_version ()
{
  return NSAppKitVersionNumber;
}


/************************************************************************
			     Application
 ************************************************************************/

extern int menu_item_selection;
extern int mac_pass_command_to_system;
extern int mac_pass_control_to_system;

static void init_menu_bar P_ ((void));
static void init_apple_event_handler P_ ((void));
static UInt32 mac_modifier_flags_to_modifiers P_ ((NSUInteger));

static BOOL is_action_selector P_ ((SEL));
static BOOL is_services_handler_selector P_ ((SEL));
static NSMethodSignature *action_signature P_ ((void));
static NSMethodSignature *services_handler_signature P_ ((void));
static void handle_action_invocation P_ ((NSInvocation *));
static void handle_services_invocation P_ ((NSInvocation *));

extern struct frame *mac_focus_frame P_ ((struct mac_display_info *));
extern void do_keystroke P_ ((EventKind, unsigned char, UInt32, UInt32,
			      unsigned long, struct input_event *));
extern UInt32 mac_mapped_modifiers P_ ((UInt32, UInt32));

@implementation EmacsApplication

/* Don't use the "applicationShouldTerminate: - NSTerminateLater -
   replyToApplicationShouldTerminate:" mechanism provided by
   -[NSApplication terminate:] for deferring the termination, as it
   does not allow us to go back to the Lisp evaluation loop.  */

- (void)terminate:(id)sender
{
  OSErr err;
  NSAppleEventManager *manager = [NSAppleEventManager sharedAppleEventManager];
  AppleEvent appleEvent, reply;

  err = create_apple_event (kCoreEventClass, kAEQuitApplication, &appleEvent);
  if (err == noErr)
    {
      AEInitializeDesc (&reply);
      [manager dispatchRawAppleEvent:&appleEvent withRawReply:&reply
	       handlerRefCon:0];
      AEDisposeDesc (&reply);
      AEDisposeDesc (&appleEvent);
    }
}

@end				// EmacsApplication


@implementation EmacsController

/* Delegete Methods  */

- (void)applicationWillFinishLaunching:(NSNotification *)aNotification
{
  [EmacsPosingWindow setup];
  [NSFontManager setFontPanelFactory:[EmacsFontPanel class]];
  init_menu_bar ();
  init_apple_event_handler ();
}

- (void)applicationDidFinishLaunching:(NSNotification *)aNotification
{
  /* Try to suppress the warning "CFMessagePort: bootstrap_register():
     failed" displayed by the second instance of Emacs.  Strictly
     speaking, there's a race condition, but it is not critical
     anyway.  Unfortunately, Mac OS X 10.4 still displays warnings at
     -[NSApplication setServicesMenu:] or the first event loop.  */
  if (!mac_service_provider_registered_p ())
    [NSApp setServicesProvider:self];

  /* Dummy object creation/destruction so +[NSTSMInputContext
     initialize] can install a handler to the event dispatcher target
     before install_dispatch_handler does that.  */
  [[[(NSClassFromString (@"NSTSMInputContext")) alloc] init] release];
  install_dispatch_handler ();

  /* Exit from the main event loop.  */
  [NSApp stop:nil];
  [NSApp postDummyEvent];
}

/* Action methods  */

/* Store SENDER's inputEvent to kbd_buffer.  */

- (void)storeInputEvent:(id)sender
{
  [self storeEvent:[sender inputEvent]];
}

/* Set the global variable menu_item_selection to the value of
   SENDER's tag.  */

- (void)setMenuItemSelectionToTag:(id)sender
{
  menu_item_selection = [sender tag];
}

/* Event handling  */

static EventRef peek_next_event P_ ((void));
static EventRef peek_if_next_event_activates_menu_bar P_ ((void));

/* Store BUFP to kbd_buffer.  */

- (void)storeEvent:(struct input_event *)bufp
{
  if (bufp->kind == HELP_EVENT)
    {
      do_help = 1;
      emacsHelpFrame = XFRAME (bufp->frame_or_window);
    }
  else
    {
      kbd_buffer_store_event_hold (bufp, hold_quit);
      count++;
    }
}

- (void)setTrackingObject:(id)object andResumeSelector:(SEL)selector
{
  if (trackingObject != object)
    {
      [trackingObject release];
      trackingObject = [object retain];
    }

  trackingResumeSelector = selector;
}

/* Handle the NSEvent EVENT.  */

- (void)handleOneNSEvent:(NSEvent *)event
{
  struct mac_display_info *dpyinfo = &one_mac_display_info;
  struct input_event inev;

  do_help = 0;
  emacsHelpFrame = NULL;

  EVENT_INIT (inev);
  inev.arg = Qnil;
  XSETFRAME (inev.frame_or_window, mac_focus_frame (dpyinfo));

  switch ([event type])
    {
    case NSKeyDown:
      {
	NSUInteger flags = [event modifierFlags];
	UInt32 modifiers = mac_modifier_flags_to_modifiers (flags);
	unsigned short key_code = [event keyCode];
	NSString *characters;
	unsigned char char_code;

	if (!(mac_mapped_modifiers (modifiers, key_code)
	      & ~(mac_pass_command_to_system ? cmdKey : 0)
	      & ~(mac_pass_control_to_system ? controlKey : 0))
	    && ([NSApp keyWindow] || (flags & NSCommandKeyMask)))
	  {
#if MAC_OS_X_VERSION_MIN_REQUIRED < 1050
	    /* This is a workaround for the problem that Control-Tab
	       is not recognized on Mac OS X 10.4 and earlier.  */
	    if (floor (NSAppKitVersionNumber) <= NSAppKitVersionNumber10_4
		&& [[[NSApp keyWindow] firstResponder]
		     isMemberOfClass:[EmacsView class]]
		&& key_code == 0x30 /* kVK_Tab */
		&& ((flags & (NSControlKeyMask | NSCommandKeyMask))
		    == NSControlKeyMask)
		&& [[NSApp mainMenu] performKeyEquivalent:event])
	      break;
#endif
	    goto OTHER;
	  }

	characters = [event characters];
	if ([characters length] == 1 && [characters characterAtIndex:0] < 0x80)
	  char_code = [characters characterAtIndex:0];
	else
	  char_code = 0;

	do_keystroke (([event isARepeat] ? autoKey : keyDown),
		      char_code, key_code, modifiers,
		      [event timestamp] * 1000, &inev);

	[self storeEvent:&inev];
      }
      break;

    default:
    OTHER:
      [NSApp sendEvent:event];
      break;
    }

  if (do_help
      && !(hold_quit && hold_quit->kind != NO_EVENT))
    {
      Lisp_Object frame;

      if (emacsHelpFrame)
	XSETFRAME (frame, emacsHelpFrame);
      else
	frame = Qnil;

      if (do_help > 0)
	{
	  any_help_event_p = 1;
	  gen_help_event (help_echo_string, frame, help_echo_window,
			  help_echo_object, help_echo_pos);
	}
      else
	{
	  help_echo_string = Qnil;
	  gen_help_event (Qnil, frame, Qnil, Qnil, 0);
	}
      count++;
    }
}

/* Handle NSEvents in the queue with holding quit event in *BUFP.
   Return the number of stored Emacs events.

   We handle them inside the application loop in order to avoid the
   hang in the following situation:

     1. Save some file in Emacs.
     2. Remove the file in Terminal.
     3. Try to drag the proxy icon in the Emacs title bar.
     4. "Document Drag Error" window will pop up, but can't pop it
        down by clicking the OK button.  */

- (int)handleQueuedNSEventsWithHoldingQuitIn:(struct input_event *)bufp
{
  if ([NSApp isRunning])
    {
      /* Mac OS X 10.2 doesn't regard untilDate:nil as polling.  */
      NSDate *expiration = [NSDate distantPast];
      struct mac_display_info *dpyinfo = &one_mac_display_info;

      hold_quit = bufp;
      count = 0;

      while (1)
	{
	  NSEvent *event;
	  NSUInteger mask;

	  if (trackingObject)
	    {
	      NSEvent *event =
		[NSApp nextEventMatchingMask:
			 (NSLeftMouseDraggedMask|NSLeftMouseUpMask)
		       untilDate:expiration
		       inMode:NSDefaultRunLoopMode dequeue:NO];

	      if (event)
		{
		  if ([event type] == NSLeftMouseDragged)
		    [trackingObject performSelector:trackingResumeSelector];
		  [self setTrackingObject:nil
			andResumeSelector:@selector(dummy)];
		}
	    }
	  else if (dpyinfo->saved_menu_event == NULL)
	    {
	      EventRef menu_event = peek_if_next_event_activates_menu_bar ();

	      if (menu_event)
		{
		  struct input_event inev;

		  dpyinfo->saved_menu_event = RetainEvent (menu_event);
		  RemoveEventFromQueue (GetMainEventQueue (), menu_event);

		  EVENT_INIT (inev);
		  inev.arg = Qnil;
		  XSETFRAME (inev.frame_or_window, mac_focus_frame (dpyinfo));
		  inev.kind = MENU_BAR_ACTIVATE_EVENT;
		  [self storeEvent:&inev];
		}
	    }

	  mask = (trackingObject == nil || dpyinfo->saved_menu_event == NULL
		  ? NSAnyEventMask : (NSAnyEventMask & ~ANY_MOUSE_EVENT_MASK));
	  event = [NSApp nextEventMatchingMask:mask untilDate:expiration
			 inMode:NSDefaultRunLoopMode dequeue:YES];

	  if (event == nil)
	    break;
	  [self handleOneNSEvent:event];
	}

      hold_quit = NULL;

      return count;
    }
  else
    {
      static NSInvocation *invocation = nil;
      int result;

      /* Cache the NSInvocation object because it is repeatedly used
	 and the EmacsController object is singleton.  */
      if (invocation == nil)
	{
	  NSMethodSignature *signature = [self methodSignatureForSelector:_cmd];

	  invocation = [NSInvocation invocationWithMethodSignature:signature];
	  [invocation setTarget:self];
	  [invocation setSelector:_cmd];
	  [invocation retain];
	}
      [invocation setArgument:&bufp atIndex:2];

      [NSApp runTemporarilyWithInvocation:invocation];

      [invocation getReturnValue:&result];

      return result;
    }
}

static BOOL
emacs_windows_need_display_p (with_resize_control_p)
     int with_resize_control_p;
{
  Lisp_Object tail, frame;

  FOR_EACH_FRAME (tail, frame)
    {
      struct frame *f = XFRAME (frame);

      if (FRAME_MAC_P (f))
	{
	  EmacsWindow *window = FRAME_MAC_WINDOW (f);

	  if ((with_resize_control_p && [window resizeControlNeedsDisplay])
	      || [window viewsNeedDisplay])
	    return YES;
	}
    }

  return NO;
}

- (void)processDeferredReadSocket:(NSTimer *)theTimer
{
  if (![NSApp isRunning])
    {
      if (peek_next_event () || emacs_windows_need_display_p (1))
	[NSApp postDummyEvent];
      else
	x_flush (NULL);
    }
}

/* Some key bindings in mac_apple_event_map are regarded as methods in
   the application delegate.  */

- (BOOL)respondsToSelector:(SEL)aSelector
{
  return ([super respondsToSelector:aSelector]
	  || is_action_selector (aSelector)
	  || is_services_handler_selector (aSelector));
}

- (NSMethodSignature *)methodSignatureForSelector:(SEL)aSelector
{
  NSMethodSignature *signature = [super methodSignatureForSelector:aSelector];

  if (signature)
    return signature;
  else if (is_action_selector (aSelector))
    return action_signature ();
  else if (is_services_handler_selector (aSelector))
    return services_handler_signature ();
  else
    return nil;
}

- (void)forwardInvocation:(NSInvocation *)anInvocation
{
  SEL selector = [anInvocation selector];
  NSMethodSignature *signature = [anInvocation methodSignature];

  if (is_action_selector (selector)
      && [signature isEqual:(action_signature ())])
    handle_action_invocation (anInvocation);
  else if (is_services_handler_selector (selector)
	   && [signature isEqual:(services_handler_signature ())])
    handle_services_invocation (anInvocation);
  else
    [super forwardInvocation:anInvocation];
}

- (BOOL)validateUserInterfaceItem:(id <NSValidatedUserInterfaceItem>)anItem
{
  return is_action_selector ([anItem action]);
}

@end				// EmacsController

OSStatus
install_application_handler ()
{
  [EmacsApplication sharedApplication];
  [NSApp setDelegate:[[EmacsController alloc] init]];

  /* Will be stopped at applicationDidFinishLaunching: in the
     delegate.  */
  [NSApp run];

  return noErr;
}


/************************************************************************
			       Windows
 ************************************************************************/

static void set_global_focus_view_frame P_ ((struct frame *));
static void unset_global_focus_view_frame P_ ((void));

extern void mac_handle_visibility_change P_ ((struct frame *));
extern void mac_handle_origin_change P_ ((struct frame *));
extern void mac_handle_size_change P_ ((struct frame *, int, int));

extern void mac_focus_changed P_ ((int, struct mac_display_info *,
				   struct frame *, struct input_event *));
extern OSStatus mac_restore_keyboard_input_source P_ ((void));
extern void mac_save_keyboard_input_source P_ ((void));

static OSStatus mac_create_frame_tool_bar P_ ((FRAME_PTR f));

#define DEFAULT_NUM_COLS (80)
#define RESIZE_CONTROL_WIDTH (15)
#define RESIZE_CONTROL_HEIGHT (15)

@implementation EmacsWindow

- (id)initWithContentRect:(NSRect)contentRect
		styleMask:(NSUInteger)windowStyle
		  backing:(NSBackingStoreType)bufferingType
		    defer:(BOOL)deferCreation
{
  self = [super initWithContentRect:contentRect styleMask:windowStyle
			    backing:bufferingType defer:deferCreation];
  if (self == nil)
    return nil;

  [[NSNotificationCenter defaultCenter]
    addObserver:self
    selector:@selector(applicationDidUnhide:)
    name:NSApplicationDidUnhideNotification
    object:NSApp];

  return self;
}

- (void)dealloc
{
  [[NSNotificationCenter defaultCenter] removeObserver:self];
  [super dealloc];
}

- (NSRect)resizeControlFrame
{
  NSRect frame = [self frame];
  CGFloat scaleFactor;

  if ([self respondsToSelector:@selector(userSpaceScaleFactor)])
    scaleFactor = [self userSpaceScaleFactor];
  else
    scaleFactor = 1.0;

  if (scaleFactor == 1.0)
    return NSMakeRect (NSWidth (frame) - RESIZE_CONTROL_WIDTH, 0,
		       RESIZE_CONTROL_WIDTH, RESIZE_CONTROL_HEIGHT);
  else
    {
      CGFloat width, height;

      width = round (RESIZE_CONTROL_WIDTH * scaleFactor);
      height = round (RESIZE_CONTROL_HEIGHT * scaleFactor);

      return NSMakeRect (NSWidth (frame) - width, 0, width, height);
    }
}

- (void)setupResizeTracking:(NSEvent *)event
{
  NSRect resizeControlFrame = [self resizeControlFrame];
  NSPoint location = [event locationInWindow];

  resizeTrackingOffset = NSMakePoint (location.x - NSMinX (resizeControlFrame),
				      location.y - NSMinY (resizeControlFrame));
}

- (void)suspendResizeTracking:(NSEvent *)event
{
  mouseUpEvent = [[event mouseEventByChangingType:NSLeftMouseUp
			 andLocation:[event locationInWindow]] retain];
  [NSApp postEvent:mouseUpEvent atStart:YES];
  /* Use notification?  */
  [[NSApp delegate] setTrackingObject:self
		    andResumeSelector:@selector(resumeResizeTracking)];
}

- (void)resumeResizeTracking
{
  NSRect resizeControlFrame = [self resizeControlFrame];
  NSPoint location =
    NSMakePoint (NSMinX (resizeControlFrame) + resizeTrackingOffset.x,
		 NSMinY (resizeControlFrame) + resizeTrackingOffset.y);
  NSEvent *mouseDownEvent =
    [mouseUpEvent mouseEventByChangingType:NSLeftMouseDown
		  andLocation:location];

  [mouseUpEvent release];
  mouseUpEvent = nil;
  [NSApp postEvent:mouseDownEvent atStart:YES];
}

- (void)sendEvent:(NSEvent *)event
{
  if ([event type] == NSLeftMouseDown
      && NSMouseInRect ([event locationInWindow],
			[self resizeControlFrame], NO))
    [self setupResizeTracking:event];

  [super sendEvent:event];
}

- (BOOL)resizeControlNeedsDisplay
{
  return resizeControlNeedsDisplay;
}

- (void)setResizeControlNeedsDisplay:(BOOL)flag
{
  resizeControlNeedsDisplay = flag;
}

- (void)displayResizeControlIfNeeded
{
  if (resizeControlNeedsDisplay)
    {
      NSView *frameView = [[self contentView] superview];
      NSRect rect = [frameView convertRect:[self resizeControlFrame]
			       fromView:nil];

      [frameView displayRect:rect];
      resizeControlNeedsDisplay = NO;
    }
}

- (BOOL)needsOrderFrontOnUnhide;
{
  return needsOrderFrontOnUnhide;
}

- (void)setNeedsOrderFrontOnUnhide:(BOOL)flag
{
  needsOrderFrontOnUnhide = flag;
}

- (void)applicationDidUnhide:(NSNotification *)aNotification
{
  if (needsOrderFrontOnUnhide)
    {
      [self orderFront:nil];
      needsOrderFrontOnUnhide = NO;
    }
}

@end				// EmacsWindow

@implementation EmacsFrameController

- (id)initWithEmacsFrame:(struct frame *)f
{
  self = [self init];
  if (self == nil)
    return nil;

  emacsFrame = f;

  return self;
}

- (struct frame *)emacsFrame
{
  return emacsFrame;
}

/* Delegete Methods.  */

- (void)windowDidBecomeKey:(NSNotification *)aNotification
{
  struct frame *f = emacsFrame;
  struct input_event inev;

  EVENT_INIT (inev);
  inev.arg = Qnil;
  mac_focus_changed (activeFlag, FRAME_MAC_DISPLAY_INFO (f), f, &inev);
  if (inev.kind != NO_EVENT)
    [[NSApp delegate] storeEvent:&inev];
}

- (void)windowDidResignKey:(NSNotification *)aNotification
{
  struct frame *f = emacsFrame;
  struct input_event inev;

  EVENT_INIT (inev);
  inev.arg = Qnil;
  mac_focus_changed (0, FRAME_MAC_DISPLAY_INFO (f), f, &inev);
  if (inev.kind != NO_EVENT)
    [[NSApp delegate] storeEvent:&inev];
}

- (void)windowDidBecomeMain:(NSNotification *)aNotification
{
  mac_restore_keyboard_input_source ();
}

- (void)windowDidResignMain:(NSNotification *)aNotification
{
  struct frame *f = emacsFrame;
  EmacsView *emacsView = FRAME_EMACS_VIEW (f);

  [emacsView unmarkText];
  [[NSInputManager currentInputManager] markedTextAbandoned:emacsView];
  mac_save_keyboard_input_source ();
}

- (void)windowDidMove:(NSNotification *)aNotification
{
  struct frame *f = emacsFrame;

  mac_handle_origin_change (f);
}

- (void)windowDidResize:(NSNotification *)aNotification
{
  struct frame *f = emacsFrame;
  int x, y;

  x_real_positions (f, &x, &y);
}

- (BOOL)windowShouldClose:(id)sender
{
  struct frame *f = emacsFrame;
  struct input_event inev;

  EVENT_INIT (inev);
  inev.arg = Qnil;
  inev.kind = DELETE_WINDOW_EVENT;
  XSETFRAME (inev.frame_or_window, f);
  [[NSApp delegate] storeEvent:&inev];

  return NO;
}

- (void)windowWillClose:(NSNotification *)aNotification
{
  NSWindow *window = [aNotification object];

  [window setDelegate:nil];
  [self release];
}

- (void)windowWillMove:(NSNotification *)aNotification
{
  struct frame *f = emacsFrame;

  f->output_data.mac->toolbar_win_gravity = 0;
}

- (NSSize)windowWillResize:(NSWindow *)sender
		    toSize:(NSSize)proposedFrameSize
{
  struct frame *f = emacsFrame;
  NSEvent *currentEvent = [NSApp currentEvent];
  BOOL leftMouseDragged = ([currentEvent type] == NSLeftMouseDragged);
  XSizeHints *size_hints = FRAME_SIZE_HINTS (f);
  EmacsView *emacsView = FRAME_EMACS_VIEW (f);
  NSRect windowFrame, emacsViewFrame;
  NSSize emacsViewSizeInPixels, emacsViewSize;
  CGFloat dw, dh;

  if (leftMouseDragged)
    {
      EmacsWindow *window = (EmacsWindow *) sender;

      [window suspendResizeTracking:currentEvent];
    }

  windowFrame = [sender frame];
  if (size_hints == NULL)
    return windowFrame.size;

  emacsViewFrame = [emacsView frame];
  emacsViewSizeInPixels = [emacsView convertSize:emacsViewFrame.size
				     toView:nil];
  dw = NSWidth (windowFrame) - emacsViewSizeInPixels.width;
  dh = NSHeight (windowFrame) - emacsViewSizeInPixels.height;
  emacsViewSize =
    [emacsView convertSize:(NSMakeSize (proposedFrameSize.width - dw,
					proposedFrameSize.height - dh))
	       fromView:nil];

  if (emacsViewSize.width < size_hints->min_width)
    emacsViewSize.width = size_hints->min_width;
  else
    emacsViewSize.width = size_hints->base_width
      + (int) ((emacsViewSize.width - size_hints->base_width)
	       / size_hints->width_inc + (leftMouseDragged ? .5 : 0))
      * size_hints->width_inc;

  if (emacsViewSize.height < size_hints->min_height)
    emacsViewSize.height = size_hints->min_height;
  else
    emacsViewSize.height = size_hints->base_height
      + (int) ((emacsViewSize.height - size_hints->base_height)
	       / size_hints->height_inc + (leftMouseDragged ? .5 : 0))
      * size_hints->height_inc;

  emacsViewSizeInPixels = [emacsView convertSize:emacsViewSize toView:nil];

  return NSMakeSize (emacsViewSizeInPixels.width + dw,
		     emacsViewSizeInPixels.height + dh);
}

- (NSRect)windowWillUseStandardFrame:(NSWindow *)sender
			defaultFrame:(NSRect)defaultFrame
{
  struct frame *f = emacsFrame;
  EmacsView *emacsView = FRAME_EMACS_VIEW (f);
  NSRect windowFrame, emacsViewFrame;
  NSSize emacsViewSizeInPixels, emacsViewSize;
  CGFloat dw, dh, dx, dy;
  int columns, rows;

  windowFrame = [sender frame];
  emacsViewFrame = [emacsView frame];
  emacsViewSizeInPixels = [emacsView convertSize:emacsViewFrame.size
				     toView:nil];
  dw = NSWidth (windowFrame) - emacsViewSizeInPixels.width;
  dh = NSHeight (windowFrame) - emacsViewSizeInPixels.height;
  emacsViewSize =
    [emacsView convertSize:(NSMakeSize (NSWidth (defaultFrame) - dw,
					NSHeight (defaultFrame) - dh))
	       fromView:nil];

  columns = FRAME_PIXEL_WIDTH_TO_TEXT_COLS (f, emacsViewSize.width);
  rows = FRAME_PIXEL_HEIGHT_TO_TEXT_LINES (f, emacsViewSize.height);
  if (columns > DEFAULT_NUM_COLS)
    columns = DEFAULT_NUM_COLS;
  emacsViewSize.width = FRAME_TEXT_COLS_TO_PIXEL_WIDTH (f, columns);
  emacsViewSize.height = FRAME_TEXT_LINES_TO_PIXEL_HEIGHT (f, rows);
  emacsViewSizeInPixels = [emacsView convertSize:emacsViewSize toView:nil];
  windowFrame.size.width = emacsViewSizeInPixels.width + dw;
  windowFrame.size.height = emacsViewSizeInPixels.height + dh;

  dx = NSMaxX (defaultFrame) - NSMaxX (windowFrame);
  if (dx < 0)
    windowFrame.origin.x += dx;
  dx = NSMinX (defaultFrame) - NSMinX (windowFrame);
  if (dx > 0)
    windowFrame.origin.x += dx;
  dy = NSMaxY (defaultFrame) - NSMaxY (windowFrame);
  if (dy > 0)
    windowFrame.origin.y += dy;

  return windowFrame;
}

@end				// EmacsFrameController


/* Window Manager function replacements.  */

void
mac_set_window_title (window, string)
     Window window;
     CFStringRef string;
{
  [(NSWindow *)window setTitle:((NSString *) string)];
}

void
mac_set_window_modified (window, modified)
     Window window;
     Boolean modified;
{
  [(NSWindow *)window setDocumentEdited:modified];
}

Boolean
mac_is_window_visible (window)
     Window window;
{
  return [(NSWindow *)window isVisible];
}

Boolean
mac_is_window_collapsed (window)
     Window window;
{
  return [(NSWindow *)window isMiniaturized];
}

void
mac_bring_window_to_front (window)
     Window window;
{
  if (![NSApp isHidden])
    [(NSWindow *)window orderFront:nil];
  else
    [(EmacsWindow *)window setNeedsOrderFrontOnUnhide:YES];
}

void
mac_send_window_behind (window, behind_window)
     Window window, behind_window;
{
  NSInteger otherWindowNumber = [(NSWindow *)behind_window windowNumber];

  [(NSWindow *)window orderWindow:NSWindowBelow relativeTo:otherWindowNumber];
}

void
mac_hide_window (window)
     Window window;
{
  [(NSWindow *)window orderOut:nil];
  [(EmacsWindow *)window setNeedsOrderFrontOnUnhide:NO];
}

void
mac_show_window (window)
     Window window;
{
  if (![(NSWindow *)window isVisible])
    {
      mac_bring_window_to_front (window);
      [(NSWindow *)window makeKeyWindow];
    }
}

OSStatus
mac_collapse_window (window, collapse)
     Window window;
     Boolean collapse;
{
  if (collapse && ![(NSWindow *)window isMiniaturized])
    [(NSWindow *)window miniaturize:nil];
  else if (!collapse && [(NSWindow *)window isMiniaturized])
    [(NSWindow *)window deminiaturize:nil];

  return noErr;
}

Window
mac_front_non_floating_window ()
{
  NSArray *orderedWindows = [NSApp orderedWindows];

  if ([orderedWindows count] > 0)
    return [orderedWindows objectAtIndex:0];
  else
    return nil;
}

Window
mac_active_non_floating_window ()
{
  return [NSApp mainWindow];
}

void
mac_activate_window (window, activate)
     Window window;
     Boolean activate;
{
  if (activate)
    [(NSWindow *)window makeKeyWindow];
}

static NSRect
mac_get_base_screen_frame ()
{
  NSArray *screens = [NSScreen screens];

  if ([screens count] > 0)
    return [[screens objectAtIndex:0] frame];
  else
    return [[NSScreen mainScreen] frame];
}

OSStatus
mac_move_window_structure (window, h, v)
     Window window;
     short h, v;
{
  NSRect baseScreenFrame = mac_get_base_screen_frame ();
  NSPoint topLeft = NSMakePoint (h + NSMinX (baseScreenFrame),
				 -v + NSMaxY (baseScreenFrame));

  [(NSWindow *)window setFrameTopLeftPoint:topLeft];

  return noErr;
}

void
mac_move_window (window, h, v, front)
     Window window;
     short h, v;
     Boolean front;
{
  NSView *contentView = [(NSWindow *)window contentView];
  NSRect contentViewFrame, baseScreenFrame;
  NSPoint windowFrameOrigin;

  contentViewFrame = [contentView convertRect:[contentView frame] toView:nil];
  baseScreenFrame = mac_get_base_screen_frame ();
  windowFrameOrigin.x = (h - NSMinX (contentViewFrame)
			 + NSMinX (baseScreenFrame));
  windowFrameOrigin.y = (-(v + NSMaxY (contentViewFrame))
			 + NSMaxY (baseScreenFrame));

  [(NSWindow *)window setFrameOrigin:windowFrameOrigin];
}

void
mac_size_window (window, w, h, update)
     Window window;
     short w, h;
     Boolean update;
{
  NSView *contentView;
  NSRect contentViewFrame, windowFrame;
  NSSize oldSizeInPixels, newSizeInPixels;
  CGFloat dw, dh;

  /* W and H are dimensions in user space coordinates; they are not
     the same as those in device space coordinates if scaling is in
     effect.  */
  contentView = [(NSWindow *)window contentView];
  contentViewFrame = [contentView frame];
  oldSizeInPixels = [contentView convertSize:contentViewFrame.size toView:nil];
  newSizeInPixels = [contentView convertSize:(NSMakeSize (w, h)) toView:nil];
  dw = newSizeInPixels.width - oldSizeInPixels.width;
  dh = newSizeInPixels.height - oldSizeInPixels.height;

  windowFrame = [(NSWindow *)window frame];
  windowFrame.origin.y -= dh;
  windowFrame.size.width += dw;
  windowFrame.size.height += dh;

  [(NSWindow *)window setFrame:windowFrame display:update];
}

OSStatus
mac_set_window_alpha (window, alpha)
     Window window;
     CGFloat alpha;
{
  [(NSWindow *)window setAlphaValue:alpha];

  return noErr;
}

void
mac_get_window_bounds (f, inner, outer)
     struct frame *f;
     Rect *inner, *outer;
{
  NSWindow *window = FRAME_MAC_WINDOW (f);
  EmacsView *emacsView = FRAME_EMACS_VIEW (f);
  NSRect baseScreenFrame = mac_get_base_screen_frame ();
  NSRect windowFrame, emacsViewFrame;

  windowFrame = [window frame];
  emacsViewFrame = [emacsView convertRect:[emacsView frame] toView:nil];
  emacsViewFrame.origin = [window convertBaseToScreen:emacsViewFrame.origin];

  SetRect (outer,
	   NSMinX (windowFrame) + NSMinX (baseScreenFrame),
	   - NSMaxY (windowFrame) + NSMaxY (baseScreenFrame),
	   NSMaxX (windowFrame) + NSMinX (baseScreenFrame),
	   - NSMinY (windowFrame) + NSMaxY (baseScreenFrame));
  SetRect (inner,
	   NSMinX (emacsViewFrame) + NSMinX (baseScreenFrame),
	   - NSMaxY (emacsViewFrame) + NSMaxY (baseScreenFrame),
	   NSMaxX (emacsViewFrame) + NSMinX (baseScreenFrame),
	   - NSMinY (emacsViewFrame) + NSMaxY (baseScreenFrame));
}

void
mac_get_frame_mouse (f, point)
     struct frame *f;
     Point *point;
{
  EmacsView *emacsView = FRAME_EMACS_VIEW (f);
  NSWindow *window = [emacsView window];
  NSPoint mouseLocation = [window mouseLocationOutsideOfEventStream];

  mouseLocation = [emacsView convertPoint:mouseLocation fromView:nil];
  SetPt (point, mouseLocation.x, mouseLocation.y);
}

void
mac_get_global_mouse (point)
     Point *point;
{
  NSPoint mouseLocation = [NSEvent mouseLocation];
  NSRect baseScreenFrame = mac_get_base_screen_frame ();

  SetPt (point,
	 mouseLocation.x + NSMinX (baseScreenFrame),
	 - mouseLocation.y + NSMaxY (baseScreenFrame));
}

void
mac_convert_frame_point_to_global (f, x, y)
     struct frame *f;
     int *x, *y;
{
  NSWindow *window = FRAME_MAC_WINDOW (f);
  EmacsView *emacsView = FRAME_EMACS_VIEW (f);
  NSPoint point = NSMakePoint (*x, *y);
  NSRect baseScreenFrame = mac_get_base_screen_frame ();

  point = [emacsView convertPoint:point toView:nil];
  point = [window convertBaseToScreen:point];
  *x = point.x + NSMinX (baseScreenFrame);
  *y = - point.y + NSMaxY (baseScreenFrame);
}

CGRect
mac_rect_make (f, x, y, w, h)
     struct frame *f;
     CGFloat x, y, w, h;
{
  NSWindow *window = FRAME_MAC_WINDOW (f);
  CGFloat scaleFactor;

  if ([window respondsToSelector:@selector(userSpaceScaleFactor)])
    scaleFactor = [window userSpaceScaleFactor];
  else
    scaleFactor = 1.0;

  if (scaleFactor == 1.0)
    return CGRectMake (x, y, w, h);
  else
    {
      EmacsView *emacsView = FRAME_EMACS_VIEW (f);
      NSRect rect = NSMakeRect (x, y, w, h);

      rect = [emacsView convertRect:rect toView:nil];
      x = round (rect.origin.x);
      y = round (rect.origin.y);
      rect.size.width = round (NSMaxX (rect)) - x;
      rect.size.height = round (NSMaxY (rect)) - y;
      rect.origin.x = x;
      rect.origin.y = y;
      rect = [emacsView convertRect:rect fromView:nil];

      return NSRectToCGRect (rect);
    }
}

void
mac_update_proxy_icon (f)
     struct frame *f;
{
  Lisp_Object file_name =
    XBUFFER (XWINDOW (FRAME_SELECTED_WINDOW (f))->buffer)->filename;
  NSWindow *window = FRAME_MAC_WINDOW (f);
  NSString *old = [window representedFilename], *new;

  if ([old length] == 0 && !STRINGP (file_name))
    return;

  if (!STRINGP (file_name))
    new = @"";
  else
    {
      new = [NSString stringWithLispString:file_name];
      if (![[NSFileManager defaultManager] fileExistsAtPath:new])
	new = @"";
      if ([new isEqualToString:old])
	new = nil;
    }

  if (new)
    [window setRepresentedFilename:new];
}

void
mac_set_frame_window_background (f, color)
     struct frame *f;
     unsigned long color;
{
  NSWindow *window = FRAME_MAC_WINDOW (f);
  CGFloat red, green, blue;

  red = RED_FROM_ULONG (color) / 255.0;
  green = GREEN_FROM_ULONG (color) / 255.0;
  blue = BLUE_FROM_ULONG (color) / 255.0;

  [window setBackgroundColor:[NSColor colorWithCalibratedRed:red
				      green:green blue:blue alpha:1.0]];
}

/* Flush display of frame F, or of all frames if F is null.  */

static struct frame *global_focus_view_frame;

void
x_flush (f)
     struct frame *f;
{
  BLOCK_INPUT;

  if (f == NULL)
    {
      Lisp_Object rest, frame;
      FOR_EACH_FRAME (rest, frame)
	if (FRAME_MAC_P (XFRAME (frame)))
	  x_flush (XFRAME (frame));
    }
  else
    {
      NSWindow *window = FRAME_MAC_WINDOW (f);

      if ([window isVisible] && ![window isFlushWindowDisabled])
	[window flushWindow];
    }

  UNBLOCK_INPUT;
}

void
mac_update_begin (f)
     struct frame *f;
{
  NSWindow *window = FRAME_MAC_WINDOW (f);
  EmacsView *emacsView = FRAME_EMACS_VIEW (f);

  [window disableFlushWindow];
  [emacsView lockFocus];
  set_global_focus_view_frame (f);
}

void
mac_update_end (f)
     struct frame *f;
{
  NSWindow *window = FRAME_MAC_WINDOW (f);
  EmacsView *emacsView = FRAME_EMACS_VIEW (f);

  unset_global_focus_view_frame ();
  [emacsView unlockFocus];
  [window enableFlushWindow];
}

void
mac_frame_up_to_date (f)
     struct frame *f;
{
  /* Redraw the resize control.  */
  if (NILP (tip_frame) || XFRAME (tip_frame) != f)
    {
      EmacsWindow *window = FRAME_MAC_WINDOW (f);

      [window setResizeControlNeedsDisplay:YES];
    }
}

/* Create a new Mac window for the frame F and store it in
   FRAME_MAC_WINDOW (f).  Non-zero TOOLTIP_P means it is for the tip
   frame.  */

void
mac_create_frame_window (f, tooltip_p)
     struct frame *f;
     int tooltip_p;
{
  NSRect contentRect;
  unsigned int windowStyle;
  BOOL deferCreation;
  NSWindow *window, *mainWindow = [NSApp mainWindow];
  EmacsView *emacsView;

  if (!tooltip_p)
    {
      contentRect = NSMakeRect (0, 0,
				FRAME_PIXEL_WIDTH (f), FRAME_PIXEL_HEIGHT (f));
      windowStyle = (NSTitledWindowMask | NSClosableWindowMask
		     | NSMiniaturizableWindowMask | NSResizableWindowMask);
      deferCreation = YES;
    }
  else
    {
      contentRect = NSMakeRect (0, 0, 100, 100);
      windowStyle = NSBorderlessWindowMask;
      deferCreation = NO;
    }

  window = [[EmacsWindow alloc] initWithContentRect:contentRect
				styleMask:windowStyle
				backing:NSBackingStoreBuffered
				defer:deferCreation];
  FRAME_MAC_WINDOW (f) = window;
  [window setDelegate:[[EmacsFrameController alloc] initWithEmacsFrame:f]];

  [window useOptimizedDrawing:YES];
  if (!tooltip_p)
    [window setAcceptsMouseMovedEvents:YES];
  else
    {
      [window setAutodisplay:NO];
      [window setHasShadow:YES];
      [window setLevel:NSScreenSaverWindowLevel];
      if ([window respondsToSelector:@selector(setIgnoresMouseEvents:)])
	[window setIgnoresMouseEvents:YES];
    }

  if (f->size_hint_flags & (USPosition | PPosition))
    mac_move_window_structure (window, f->left_pos, f->top_pos);
  else
    {
      if (mainWindow == nil)
	[window center];
      else
	{
	  NSRect windowFrame = [mainWindow frame];
	  NSPoint topLeft = NSMakePoint (NSMinX (windowFrame),
					 NSMaxY (windowFrame));

	  topLeft = [window cascadeTopLeftFromPoint:topLeft];
	  [window cascadeTopLeftFromPoint:topLeft];
	}
    }

  if (!tooltip_p)
    emacsView = [[EmacsView alloc] initWithFrame:contentRect];
  else
    emacsView = [[EmacsTipView alloc] initWithFrame:contentRect];

  FRAME_EMACS_VIEW (f) = emacsView;
  [[window contentView] addSubview:emacsView];
  [emacsView setAutoresizingMask:(NSViewMaxXMargin | NSViewMinYMargin
				  | NSViewWidthSizable | NSViewHeightSizable)];
  if (!tooltip_p)
    {
      [emacsView setAction:@selector(storeInputEvent:)];
      mac_create_frame_tool_bar (f);
    }
  [emacsView release];
}

/* Dispose of the Mac window of the frame F.  */

void
mac_dispose_frame_window (f)
     struct frame *f;
{
  NSWindow *window = FRAME_MAC_WINDOW (f);

  [window close];
}


/************************************************************************
			   View and Drawing
 ************************************************************************/

extern Lisp_Object Vmac_emulate_three_button_mouse;
extern Lisp_Object Vmac_ts_active_input_overlay;
extern Lisp_Object Qbefore_string;
extern Lisp_Object Qtext_input, Qinsert_text, Qset_marked_text;
extern int mac_wheel_button_is_mouse_2;
extern Rect last_mouse_glyph;
extern FRAME_PTR last_mouse_glyph_frame;

#ifdef __STDC__
extern int volatile input_signal_count;
#else
extern int input_signal_count;
#endif

extern struct frame *pending_autoraise_frame;
extern int mac_screen_config_changed;

extern int mac_get_emulated_btn P_ ((UInt32));
extern int mac_to_emacs_modifiers P_ ((UInt32, UInt32));

extern int fast_find_position P_ ((struct window *, EMACS_INT, int *, int *,
				   int *, int *, Lisp_Object));
extern struct glyph *x_y_to_hpos_vpos P_ ((struct window *, int, int,
					   int *, int *, int *, int *, int *));

static int note_mouse_movement P_ ((FRAME_PTR, NSPoint));
static void mac_tool_bar_note_mouse_movement P_ ((struct frame *, NSEvent *));

static int mac_get_mouse_btn P_ ((NSEvent *));
static int mac_event_to_emacs_modifiers P_ ((NSEvent *));

/* View for Emacs frame.  */

@implementation EmacsTipView

- (struct frame *)emacsFrame
{
  return [[[self window] delegate] emacsFrame];
}

- (void)drawRect:(NSRect)aRect
{
  struct frame *f = [self emacsFrame];
  int x = NSMinX (aRect), y = NSMinY (aRect);
  int width = NSWidth (aRect), height = NSHeight (aRect);

  set_global_focus_view_frame (f);
  mac_clear_area (f, x, y, width, height);
  expose_frame (f, x, y, width, height);
  unset_global_focus_view_frame ();
}

- (BOOL)isFlipped
{
  return YES;
}

- (BOOL)isOpaque
{
  return YES;
}

@end				// EmacsTipView

@implementation EmacsView

- (id)initWithFrame:(NSRect)frameRect
{
  self = [super initWithFrame:frameRect];
  if (self == nil)
    return nil;

  [[NSNotificationCenter defaultCenter]
    addObserver:self
    selector:@selector(viewFrameDidChange:)
    name:@"NSViewFrameDidChangeNotification"
    object:self];

  return self;
}

- (void)dealloc
{
  [[NSNotificationCenter defaultCenter] removeObserver:self];
  [markedText release];
  [super dealloc];
}

- (void)setMarkedText:(id)aString
{
  if (markedText == aString)
    return;

  [markedText autorelease];
  markedText = [aString copy];
}

- (BOOL)acceptsFirstResponder
{
  return YES;
}

- (id)target
{
  return target;
}

- (SEL)action
{
  return action;
}

- (void)setTarget:(id)anObject
{
  target = anObject;		/* Targets should not be retained. */
}

- (void)setAction:(SEL)aSelector
{
  action = aSelector;
}

- (BOOL)sendAction:(SEL)theAction to:(id)theTarget
{
  return [NSApp sendAction:theAction to:theTarget from:self];
}

- (struct input_event *)inputEvent
{
  return &inputEvent;
}

- (void)mouseDown:(NSEvent *)theEvent
{
  struct frame *f = [self emacsFrame];
  struct mac_display_info *dpyinfo = FRAME_MAC_DISPLAY_INFO (f);
  NSPoint point = [self convertPoint:[theEvent locationInWindow] fromView:nil];
  int tool_bar_p, down_p;

  down_p = (NSEventMaskFromType ([theEvent type]) & ANY_MOUSE_DOWN_EVENT_MASK);

  if (!down_p && !(dpyinfo->grabbed & (1 << [theEvent buttonNumber])))
    return;

  last_mouse_glyph_frame = 0;

  EVENT_INIT (inputEvent);
  inputEvent.arg = Qnil;
  inputEvent.timestamp = [theEvent timestamp] * 1000;
  inputEvent.code = mac_get_mouse_btn (theEvent);
  inputEvent.modifiers = mac_event_to_emacs_modifiers (theEvent);

  {
    Lisp_Object window;
    EMACS_INT x = point.x;
    EMACS_INT y = point.y;

    XSETINT (inputEvent.x, x);
    XSETINT (inputEvent.y, y);

    window = window_from_coordinates (f, x, y, 0, 0, 0, 1);
    if (EQ (window, f->tool_bar_window))
      {
	if (down_p)
	  handle_tool_bar_click (f, x, y, 1, 0);
	else
	  handle_tool_bar_click (f, x, y, 0, inputEvent.modifiers);
	tool_bar_p = 1;
      }
    else
      {
	XSETFRAME (inputEvent.frame_or_window, f);
	inputEvent.kind = MOUSE_CLICK_EVENT;
      }
  }

  if (down_p)
    {
      dpyinfo->grabbed |= (1 << [theEvent buttonNumber]);
      last_mouse_frame = f;

      if (!tool_bar_p)
	last_tool_bar_item = -1;
    }
  else
    dpyinfo->grabbed &= ~(1 << [theEvent buttonNumber]);

  /* Ignore any mouse motion that happened before this event; any
     subsequent mouse-movement Emacs events should reflect only motion
     after the ButtonPress.  */
  if (f != 0)
    f->mouse_moved = 0;

  inputEvent.modifiers |= (down_p ? down_modifier : up_modifier);
  if (inputEvent.kind == MOUSE_CLICK_EVENT)
    [self sendAction:action to:target];
}

- (void)mouseUp:(NSEvent *)theEvent
{
  [self mouseDown:theEvent];
}

- (void)rightMouseDown:(NSEvent *)theEvent
{
  [self mouseDown:theEvent];
}

- (void)rightMouseUp:(NSEvent *)theEvent
{
  [self mouseDown:theEvent];
}

- (void)otherMouseDown:(NSEvent *)theEvent
{
  [self mouseDown:theEvent];
}

- (void)otherMouseUp:(NSEvent *)theEvent
{
  [self mouseDown:theEvent];
}

- (void)scrollWheel:(NSEvent *)theEvent
{
  struct frame *f = [self emacsFrame];
  NSPoint point = [self convertPoint:[theEvent locationInWindow] fromView:nil];

  if (
#if 0 /* We let the framework decide whether events to non-focus frame
	 get accepted.  */
      f != mac_focus_frame (&one_mac_display_info) ||
#endif
      [theEvent deltaY] == 0.0f)
    return;

  if (point.x < 0 || point.y < 0
      || EQ (window_from_coordinates (f, point.x, point.y, 0, 0, 0, 1),
	     f->tool_bar_window))
    return;

  EVENT_INIT (inputEvent);
  inputEvent.arg = Qnil;
  inputEvent.kind = WHEEL_EVENT;
  inputEvent.code = 0;
  inputEvent.modifiers = (mac_event_to_emacs_modifiers (theEvent)
			  | (([theEvent deltaY] < 0)
			     ? down_modifier : up_modifier));
  XSETINT (inputEvent.x, point.x);
  XSETINT (inputEvent.y, point.y);
  XSETFRAME (inputEvent.frame_or_window, f);
  inputEvent.timestamp = [theEvent timestamp] * 1000;
  [self sendAction:action to:target];
}

- (void)mouseMoved:(NSEvent *)theEvent
{
  struct frame *f;
  struct mac_display_info *dpyinfo = FRAME_MAC_DISPLAY_INFO (f);
  NSPoint point = [self convertPoint:[theEvent locationInWindow] fromView:nil];
  static Lisp_Object last_window;

  previous_help_echo_string = help_echo_string;
  help_echo_string = Qnil;

  if (dpyinfo->grabbed && last_mouse_frame
      && FRAME_LIVE_P (last_mouse_frame))
    f = last_mouse_frame;
  else
    f = [self emacsFrame];

  if (dpyinfo->mouse_face_hidden)
    {
      dpyinfo->mouse_face_hidden = 0;
      clear_mouse_face (dpyinfo);
    }

  /* Generate SELECT_WINDOW_EVENTs when needed.  */
  if (!NILP (Vmouse_autoselect_window))
    {
      Lisp_Object window;

      window = window_from_coordinates (f, point.x, point.y, 0, 0, 0, 0);

      /* Window will be selected only when it is not selected now and
	 last mouse movement event was not in it.  Minibuffer window
	 will be selected iff it is active.  */
      if (WINDOWP (window)
	  && !EQ (window, last_window)
	  && !EQ (window, selected_window)
	  /* For click-to-focus window managers create event iff we
	     don't leave the selected frame.  */
	  && (focus_follows_mouse
	      || (EQ (XWINDOW (window)->frame,
		      XWINDOW (selected_window)->frame))))
	{
	  EVENT_INIT (inputEvent);
	  inputEvent.arg = Qnil;
	  inputEvent.kind = SELECT_WINDOW_EVENT;
	  inputEvent.frame_or_window = window;
	  [self sendAction:action to:target];
	}

      last_window=window;
    }

  if (!note_mouse_movement (f, point))
    help_echo_string = previous_help_echo_string;
  else
    mac_tool_bar_note_mouse_movement (f, theEvent);

  /* If the contents of the global variable help_echo_string has
     changed, generate a HELP_EVENT.  */
  if (!NILP (help_echo_string) || !NILP (previous_help_echo_string))
    {
      EVENT_INIT (inputEvent);
      inputEvent.arg = Qnil;
      inputEvent.kind = HELP_EVENT;
      XSETFRAME (inputEvent.frame_or_window, f);
      [self sendAction:action to:target];
    }
}

- (void)mouseDragged:(NSEvent *)theEvent
{
  [self mouseMoved:theEvent];
}

- (void)keyDown:(NSEvent *)theEvent
{
  struct frame *f = [self emacsFrame];
  struct mac_display_info *dpyinfo = FRAME_MAC_DISPLAY_INFO (f);
  UInt32 modifiers, mapped_modifiers;
  NSString *characters;
  unsigned char char_code;

  [NSCursor setHiddenUntilMouseMoves:YES];

  /* If mouse-highlight is an integer, input clears out mouse
     highlighting.  */
  if (!dpyinfo->mouse_face_hidden && INTEGERP (Vmouse_highlight)
      && !EQ (f->tool_bar_window, dpyinfo->mouse_face_window))
    {
      clear_mouse_face (dpyinfo);
      dpyinfo->mouse_face_hidden = 1;
    }

  modifiers = mac_modifier_flags_to_modifiers ([theEvent modifierFlags]);
  mapped_modifiers = mac_mapped_modifiers (modifiers, [theEvent keyCode]);

  if (!(mapped_modifiers
	& ~(mac_pass_control_to_system ? controlKey : 0)))
    {
      keyEventsInterpreted = YES;
      rawKeyEvent = theEvent;
      [self interpretKeyEvents:[NSArray arrayWithObject:theEvent]];
      rawKeyEvent = nil;
      if (keyEventsInterpreted)
	return;
    }

  if ([theEvent type] == NSKeyUp)
    return;

  characters = [theEvent characters];
  if ([characters length] == 1 && [characters characterAtIndex:0] < 0x80)
    char_code = [characters characterAtIndex:0];
  else
    char_code = 0;

  EVENT_INIT (inputEvent);
  inputEvent.arg = Qnil;
  inputEvent.timestamp = [theEvent timestamp] * 1000;
  XSETFRAME (inputEvent.frame_or_window, f);

  do_keystroke (([theEvent isARepeat] ? autoKey : keyDown),
		char_code, [theEvent keyCode], modifiers,
		[theEvent timestamp] * 1000, &inputEvent);

  [self sendAction:action to:target];
}

- (void)interpretKeyEvents:(NSArray *)eventArray
{
  static id keyBindingManager;

  if (keyBindingManager == nil)
    keyBindingManager = [(NSClassFromString (@"NSKeyBindingManager"))
			  performSelector:@selector(sharedKeyBindingManager)];

  /* Disable the effect of NSQuotedKeystrokeBinding (C-q by default)
     and NSRepeatCountBinding (none by default but user may set it to
     C-u).  Should they be restored?  */
  [keyBindingManager performSelector:@selector(setQuoteBinding:)
		     withObject:@""];
  [keyBindingManager performSelector:@selector(setArgumentBinding:)
		     withObject:@""];
  [super interpretKeyEvents:eventArray];
}

static OSStatus
get_text_input_script_language (slrec)
     ScriptLanguageRecord *slrec;
{
  OSStatus err = noErr;

  if (current_text_input_event)
    err = GetEventParameter (current_text_input_event,
			     kEventParamTextInputSendSLRec,
			     typeIntlWritingCode, NULL,
			     sizeof (ScriptLanguageRecord), NULL, slrec);
  else
    {
      slrec->fScript = GetScriptManagerVariable (smKeyScript);
#if __LP64__
      slrec->fLanguage = kTextLanguageDontCare;
#else
      slrec->fLanguage = GetScriptVariable (slrec->fScript, smScriptLang);
#endif
    }

  return err;
}

- (void)insertText:(id)aString
{
  OSStatus err;
  struct frame *f = [self emacsFrame];
  NSString *charactersForASCIIKeystroke = nil;
  Lisp_Object arg = Qnil;
  ScriptLanguageRecord slrec;

  if (rawKeyEvent && ![self hasMarkedText])
    {
      NSUInteger flags = [rawKeyEvent modifierFlags];
      UInt32 modifiers = mac_modifier_flags_to_modifiers (flags);

      if (mac_mapped_modifiers (modifiers, [rawKeyEvent keyCode])
	  || [rawKeyEvent type] == NSKeyUp
	  || ([aString isKindOfClass:[NSString class]]
	      && [aString isEqualToString:[rawKeyEvent characters]]
	      && [(NSString *)aString length] == 1
	      && [aString characterAtIndex:0] < 0x80))
	{
	  /* Process it in keyDown:.  */
	  keyEventsInterpreted = NO;

	  return;
	}
    }

  [self setMarkedText:nil];

  EVENT_INIT (inputEvent);
  inputEvent.arg = Qnil;
  inputEvent.timestamp = [[NSApp currentEvent] timestamp] * 1000;
  XSETFRAME (inputEvent.frame_or_window, f);

  if ([aString isKindOfClass:[NSString class]])
    {
      NSUInteger i, length = [(NSString *)aString length];
      unichar character;

      for (i = 0; i < length; i++)
	{
	  character = [aString characterAtIndex:i];
	  if (!(character >= 0x20 && character <= 0x7f))
	    break;
	}

      if (i == length)
	{
	  /* ASCII only.  Store a text-input/insert-text event to
	     clear the marked text, and store ASCII keystroke events.  */
	  charactersForASCIIKeystroke = aString;
	  aString = @"";
	}
    }

  err = get_text_input_script_language (&slrec);
  if (err == noErr)
    {
      arg = make_unibyte_string ((char *) &slrec,
				 sizeof (ScriptLanguageRecord));
      arg = list1 (Fcons (build_string ("tssl"),
			  Fcons (build_string ("intl"), arg)));
    }

  inputEvent.kind = MAC_APPLE_EVENT;
  inputEvent.x = Qtext_input;
  inputEvent.y = Qinsert_text;
  inputEvent.arg =
    Fcons (build_string ("aevt"),
	   Fcons (Fcons (build_string ("----"),
			 Fcons (build_string ("Lisp"),
				[aString UTF16LispString])), arg));
  [self sendAction:action to:target];

  if (charactersForASCIIKeystroke)
    {
      NSUInteger i, length = [charactersForASCIIKeystroke length];

      inputEvent.kind = ASCII_KEYSTROKE_EVENT;
      for (i = 0; i < length; i++)
	{
	  inputEvent.code = [charactersForASCIIKeystroke characterAtIndex:i];
	  [self sendAction:action to:target];
	}
    }
}

- (void)doCommandBySelector:(SEL)aSelector
{
  keyEventsInterpreted = NO;
}

- (void)setMarkedText:(id)aString selectedRange:(NSRange)selRange
{
  OSStatus err;
  struct frame *f = [self emacsFrame];
  Lisp_Object arg = Qnil;
  ScriptLanguageRecord slrec;

  [self setMarkedText:aString];

  err = get_text_input_script_language (&slrec);
  if (err == noErr)
    {
      arg = make_unibyte_string ((char *) &slrec,
				 sizeof (ScriptLanguageRecord));
      arg = list1 (Fcons (build_string ("tssl"),
			  Fcons (build_string ("intl"), arg)));
    }

  arg = Fcons (Fcons (build_string ("selectedRange"),
		      Fcons (build_string ("Lisp"),
			     Fcons (make_number (selRange.location),
				    make_number (selRange.length)))), arg);

  EVENT_INIT (inputEvent);
  inputEvent.kind = MAC_APPLE_EVENT;
  inputEvent.x = Qtext_input;
  inputEvent.y = Qset_marked_text;
  inputEvent.arg = Fcons (build_string ("aevt"),
			  Fcons (Fcons (build_string ("----"),
					Fcons (build_string ("Lisp"),
					       [aString UTF16LispString])),
				 arg));
  inputEvent.timestamp = [[NSApp currentEvent] timestamp] * 1000;
  XSETFRAME (inputEvent.frame_or_window, f);
  [self sendAction:action to:target];
}

- (void)unmarkText
{
  if ([self hasMarkedText])
    [self insertText:markedText];
}

- (BOOL)hasMarkedText
{
  /* The cast below is just for determining the return type.  The
     object `markedText' might be of class NSAttributedString.

     Strictly speaking, `markedText != nil &&' is not necessary
     because message to nil is defined to return 0 as NSUInteger, but
     we keep this as markedText is likely to be nil in most cases.  */
  return markedText != nil && [(NSString *)markedText length] != 0;
}

#ifdef NSINTEGER_DEFINED
- (NSInteger)conversationIdentifier
#else
- (long)conversationIdentifier
#endif
{
  return (long) NSApp;
}

extern void mac_ax_selected_text_range P_ ((struct frame *, CFRange *));
extern int mac_store_buffer_text_to_unicode_chars P_ ((struct buffer *,
						       EMACS_INT, EMACS_INT,
						       UniChar *));

- (NSAttributedString *)attributedSubstringFromRange:(NSRange)theRange
{
  NSRange markedRange = [self markedRange];
  NSAttributedString *result = nil;

  if ([self hasMarkedText]
      && NSEqualRanges (NSUnionRange (markedRange, theRange), markedRange))
    {
      NSRange range = NSMakeRange (theRange.location - markedRange.location,
				   theRange.length);

      if ([markedText isKindOfClass:[NSAttributedString class]])
	result = [markedText attributedSubstringFromRange:range];
      else
	{
	  NSString *string = [markedText substringWithRange:range];

	  result = [[[NSAttributedString alloc] initWithString:string]
		     autorelease];
	}
    }
  else if (poll_suppress_count != 0 || NILP (Vinhibit_quit))
    {
      struct frame *f = [self emacsFrame];
      struct window *w = XWINDOW (f->selected_window);
      struct buffer *b = XBUFFER (w->buffer);

      /* Are we in a window whose display is up to date?
	 And verify the buffer's text has not changed.  */
      if (EQ (w->window_end_valid, w->buffer)
	  && XINT (w->last_modified) == BUF_MODIFF (b)
	  && XINT (w->last_overlay_modified) == BUF_OVERLAY_MODIFF (b))
	{
	  EMACS_INT start = BUF_BEGV (b) + theRange.location;
	  unichar *characters = xmalloc (theRange.length * sizeof (unichar));

	  if (mac_store_buffer_text_to_unicode_chars (b, start,
						      start + theRange.length,
						      (UniChar *) characters))
	    {
	      NSString *string = [NSString stringWithCharacters:characters
					   length:theRange.length];
	      NSMutableAttributedString *attributedString =
		[[[NSMutableAttributedString alloc] initWithString:string]
		  autorelease];
	      NSUInteger i;

	      [attributedString beginEditing];
	      for (i = 0; i < theRange.length; i++)
		{
		  NSFont *font = nil;
		  int hpos, vpos, x, y;

		  if (fast_find_position (w, start + i, &hpos, &vpos, &x, &y,
					  Qnil))
		    {
		      struct glyph_row *row = MATRIX_ROW (w->current_matrix,
							  vpos);
		      struct glyph *glyph = row->glyphs[TEXT_AREA] + hpos;

		      if (glyph->type == CHAR_GLYPH
			  && !glyph->glyph_not_available_p)
			font = [NSFont fontWithFace:(FACE_FROM_ID
						     (f, glyph->face_id))];
		    }

		  if (font == nil)
		    font = [NSFont fontWithFace:(FACE_FROM_ID
						 (f, DEFAULT_FACE_ID))];
		  [attributedString addAttribute:NSFontAttributeName value:font
				    range:(NSMakeRange (i, 1))];
		}
	      [attributedString endEditing];
	      result = attributedString;
	    }
	  xfree (characters);
	}
    }

  return result;
}

- (NSRange)markedRange
{
  NSUInteger location = NSNotFound;

  if (![self hasMarkedText])
    return NSMakeRange (NSNotFound, 0);

  if (OVERLAYP (Vmac_ts_active_input_overlay)
      && !NILP (Foverlay_get (Vmac_ts_active_input_overlay, Qbefore_string)))
    location = (marker_position (OVERLAY_START (Vmac_ts_active_input_overlay))
		- BEGV);

  /* The cast below is just for determining the return type.  The
     object `markedText' might be of class NSAttributedString.  */
  return NSMakeRange (location, [(NSString *)markedText length]);
}

- (NSRange)selectedRange
{
  NSRange result;

  mac_ax_selected_text_range ([self emacsFrame], (CFRange *) &result);

  return result;
}

- (NSRect)firstRectForCharacterRange:(NSRange)theRange
{
  NSRect rect = NSZeroRect;
  struct frame *f = NULL;
  struct window *w;
  struct glyph *glyph;
  struct glyph_row *row;
  int hpos, vpos, x, y, h;

  if (theRange.location >= NSNotFound)
    {
      /* Probably asking the location of the marked text in the echo area.  */
      if ([self hasMarkedText] && WINDOWP (echo_area_window))
	{
	  w = XWINDOW (echo_area_window);
	  f = WINDOW_XFRAME (w);
	  glyph = get_phys_cursor_glyph (w);
	  if (glyph)
	    {
	      row = MATRIX_ROW (w->current_matrix, w->phys_cursor.vpos);
	      get_phys_cursor_geometry (w, row, glyph, &x, &y, &h);

	      rect = NSMakeRect (x, y, w->phys_cursor_width, h);
	    }
	}
    }
  else
    {
      struct buffer *b;

      f = [self emacsFrame];
      w = XWINDOW (f->selected_window);
      b = XBUFFER (w->buffer);

      /* Are we in a window whose display is up to date?
	 And verify the buffer's text has not changed.  */
      if (EQ (w->window_end_valid, w->buffer)
	  && XINT (w->last_modified) == BUF_MODIFF (b)
	  && XINT (w->last_overlay_modified) == BUF_OVERLAY_MODIFF (b))
	{
	  EMACS_INT charpos = theRange.location + BUF_BEGV (b);

	  if (fast_find_position (w, charpos, &hpos, &vpos, &x, &y, Qnil))
	    {
	      row = MATRIX_ROW (w->current_matrix, vpos);
	      glyph = row->glyphs[TEXT_AREA] + hpos;

	      rect = NSMakeRect (WINDOW_TEXT_TO_FRAME_PIXEL_X (w, x),
				 WINDOW_TO_FRAME_PIXEL_Y (w, y),
				 glyph->pixel_width, row->visible_height);
	    }
	}
    }

  if (f)
    {
      EmacsView *emacsView = FRAME_EMACS_VIEW (f);

      /* Convert to the screen coordinate system.  */
      rect = [emacsView convertRect:rect toView:nil];
      rect.origin = [[emacsView window] convertBaseToScreen:rect.origin];
    }

  return rect;
}

- (NSUInteger)characterIndexForPoint:(NSPoint)thePoint
{
  NSUInteger result = NSNotFound;
  NSPoint point;
  Lisp_Object window;
  enum window_part part;
  struct frame *f = [self emacsFrame];
  struct window *w;
  struct buffer *b;
  int x, y;

  point = [[self window] convertScreenToBase:thePoint];
  point = [self convertPoint:point fromView:nil];
  x = point.x;
  y = point.y;
  window = window_from_coordinates (f, x, y, &part, 0, 0, 1);
  if (!WINDOWP (window) || !EQ (window, f->selected_window))
    return result;

  /* Convert to window-relative pixel coordinates.  */
  w = XWINDOW (window);
  frame_to_window_pixel_xy (w, &x, &y);

  /* Are we in a window whose display is up to date?
     And verify the buffer's text has not changed.  */
  b = XBUFFER (w->buffer);
  if (part == ON_TEXT
      && EQ (w->window_end_valid, w->buffer)
      && XFASTINT (w->last_modified) == BUF_MODIFF (b)
      && XFASTINT (w->last_overlay_modified) == BUF_OVERLAY_MODIFF (b))
    {
      int hpos, vpos, area;
      struct glyph *glyph;

      /* Find the glyph under X/Y.  */
      glyph = x_y_to_hpos_vpos (w, x, y, &hpos, &vpos, 0, 0, &area);

      if (glyph != NULL && area == TEXT_AREA)
	result = glyph->charpos - BUF_BEGV (b);
    }

  return result;
}

- (NSArray *)validAttributesForMarkedText
{
  return nil;
}

- (NSString *)string
{
  struct frame *f;
  struct buffer *b;
  NSUInteger length;
  unichar *characters;
  NSString *result = nil;

#if MAC_OS_X_VERSION_MAX_ALLOWED >= 1030
  if (handling_document_access_lock_document_p)
    return nil;
#endif

  if (poll_suppress_count == 0 && !NILP (Vinhibit_quit))
    /* Don't try to get buffer contents as the gap might be being
       altered. */
    return nil;

  f = [self emacsFrame];
  b = XBUFFER (XWINDOW (f->selected_window)->buffer);
  length = BUF_ZV (b) - BUF_BEGV (b);
  characters = xmalloc (length * sizeof (unichar));
  if (mac_store_buffer_text_to_unicode_chars (b, BUF_BEGV (b), BUF_ZV (b),
					      (UniChar *) characters))
    result = [NSString stringWithCharacters:characters length:length];
  xfree (characters);

  return result;
}

- (void)viewDidEndLiveResize
{
  struct frame *f = [self emacsFrame];
  NSRect frameRect = [self frame];

  [super viewDidEndLiveResize];
  mac_handle_size_change (f, NSWidth (frameRect), NSHeight (frameRect));
}

- (void)viewFrameDidChange:(NSNotification *)aNotification
{
  if (![self inLiveResize])
    {
      struct frame *f = [self emacsFrame];
      NSRect frameRect = [self frame];

      mac_handle_size_change (f, NSWidth (frameRect), NSHeight (frameRect));
    }
}

@end				// EmacsView

#define FRAME_CG_CONTEXT(f)	((f)->output_data.mac->cg_context)

/* Emacs frame containing the globally focused NSView.  */
static struct frame *global_focus_view_frame;
/* -[EmacsTipView drawRect:] might be called during update_frame.  */
static struct frame *saved_focus_view_frame;
static CGContextRef saved_focus_view_context;

static void
set_global_focus_view_frame (f)
     struct frame *f;
{
  saved_focus_view_frame = global_focus_view_frame;
  if (f != global_focus_view_frame)
    {
      if (saved_focus_view_frame)
	saved_focus_view_context = FRAME_CG_CONTEXT (saved_focus_view_frame);
      global_focus_view_frame = f;
      FRAME_CG_CONTEXT (f) = [[NSGraphicsContext currentContext] graphicsPort];
    }
}

static void
unset_global_focus_view_frame ()
{
  if (global_focus_view_frame != saved_focus_view_frame)
    {
      FRAME_CG_CONTEXT (global_focus_view_frame) = NULL;
      global_focus_view_frame = saved_focus_view_frame;
      if (global_focus_view_frame)
	FRAME_CG_CONTEXT (global_focus_view_frame) = saved_focus_view_context;
    }
  saved_focus_view_frame = NULL;
}

CGContextRef
mac_begin_cg_clip (f, gc)
     struct frame *f;
     GC gc;
{
  CGContextRef context;

  if (global_focus_view_frame != f)
    {
      EmacsView *emacsView = FRAME_EMACS_VIEW (f);

      [emacsView lockFocus];
      context = [[NSGraphicsContext currentContext] graphicsPort];
      FRAME_CG_CONTEXT (f) = context;
    }
  else
    context = FRAME_CG_CONTEXT (f);

  CGContextSaveGState (context);
  if (gc && gc->n_clip_rects)
    CGContextClipToRects (context, gc->clip_rects, gc->n_clip_rects);

  return context;
}

void
mac_end_cg_clip (f)
     struct frame *f;
{
  CGContextRestoreGState (FRAME_CG_CONTEXT (f));
  if (global_focus_view_frame != f)
    {
      EmacsView *emacsView = FRAME_EMACS_VIEW (f);

      [emacsView unlockFocus];
      FRAME_CG_CONTEXT (f) = NULL;
    }
}

#if MAC_OS_X_VERSION_MAX_ALLOWED < 1040 || (MAC_OS_X_VERSION_MIN_REQUIRED < 1040 && MAC_OS_X_VERSION_MIN_REQUIRED >= 1020)

/* Last resort for the case that neither CGContextSetBlendMode nor
   InvertRect is available for inverting rectangle.  */

void
mac_appkit_invert_rectangle (f, x, y, width, height)
     struct frame *f;
     int x, y;
     unsigned int width, height;
{
  EmacsView *emacsView = FRAME_EMACS_VIEW (f);
  CGRect rect;
  NSBitmapImageRep *bitmap;

  if (![emacsView canDraw])
    return;

  if (global_focus_view_frame != f)
    [emacsView lockFocus];

  rect = mac_rect_make (f, x, y, width, height);
  bitmap = [[NSBitmapImageRep alloc]
	     initWithFocusedViewRect:(NSRectFromCGRect (rect))];
  if (bitmap && ![bitmap isPlanar] && [bitmap samplesPerPixel] == 3)
    {
      unsigned char *data = [bitmap bitmapData];
      int i, len;
      NSAffineTransform *transform;

      /* Don't multiply by `height' as it may be different from
	 [bitmap pixelsHigh] if scaling is in effect.  */
      len = [bitmap bytesPerRow] * [bitmap pixelsHigh];
      for (i = 0; i < len; i++)
	data[i] = ~data[i];

      [NSGraphicsContext saveGraphicsState];

      transform = [NSAffineTransform transform];
      [transform translateXBy:(CGRectGetMinX (rect))
		 yBy:(CGRectGetMaxY (rect))];
      [transform scaleXBy:1.0 yBy:-1.0];
      [transform concat];

      [bitmap draw];

      [NSGraphicsContext restoreGraphicsState];
    }
  [bitmap release];

  if (global_focus_view_frame != f)
    [emacsView unlockFocus];
}
#endif

/* Mac replacement for XCopyArea: used only for scrolling.  */

void
mac_scroll_area (f, gc, src_x, src_y, width, height, dest_x, dest_y)
     struct frame *f;
     GC gc;
     int src_x, src_y;
     unsigned int width, height;
     int dest_x, dest_y;
{
  EmacsView *emacsView = FRAME_EMACS_VIEW (f);
  NSRect rect = NSMakeRect (src_x, src_y, width, height);
  NSSize offset = NSMakeSize (dest_x - src_x, dest_y - src_y);

  /* Is adjustment necessary for scaling?  */
  [emacsView scrollRect:rect by:offset];
}


/************************************************************************
			     Scroll bars
 ************************************************************************/
extern Time last_mouse_movement_time;

#define SCROLL_BAR_FIRST_DELAY 0.5
#define SCROLL_BAR_CONTINUOUS_DELAY (1.0 / 15)

@implementation NonmodalScroller

- (void)dealloc
{
  [timer release];
  [super dealloc];
}

/* Whether mouse drag on knob updates the float value.  Subclass may
   override the definition.  */

- (BOOL)dragUpdatesFloatValue
{
  return YES;
}

/* First delay in seconds for mouse tracking.  Subclass may override
   the definition.  */

- (NSTimeInterval)firstDelay
{
  return SCROLL_BAR_FIRST_DELAY;
}

/* Continuous delay in seconds for mouse tracking.  Subclass may
   override the definition.  */

- (NSTimeInterval)continuousDelay
{
  return SCROLL_BAR_CONTINUOUS_DELAY;
}

- (NSScrollerPart)hitPart
{
  return hitPart;
}

- (void)highlight:(BOOL)flag
{
  if (hitPart == NSScrollerIncrementLine
      || hitPart == NSScrollerDecrementLine)
    {
      hilightsHitPart = flag;
      [self setNeedsDisplay:YES];
    }
  else
    hilightsHitPart = NO;
}

/* This method is not documented but Cocoa seems to use this for
   drawing highlighted arrow.  */

- (void)drawArrow:(NSUInteger)position highlightPart:(NSInteger)part;
{
  if (hilightsHitPart)
    part = (hitPart == NSScrollerIncrementLine ? 0 : 1);
  else
    part = -1;

  [super drawArrow:position highlightPart:part];
}

/* Post a dummy mouse dragged event to the main event queue to notify
   timer has expired.  */

- (void)postMouseDraggedEvent:(NSTimer *)theTimer
{
  NSEvent *event = [NSEvent mouseEventWithType:NSLeftMouseDragged
			    location:[[self window]
				       mouseLocationOutsideOfEventStream]
			    modifierFlags:0 timestamp:0
			    windowNumber:[[self window] windowNumber]
			    context:[NSGraphicsContext currentContext]
			    eventNumber:0 clickCount:1 pressure:0];

  [NSApp postEvent:event atStart:NO];
  [timer release];
  timer = nil;
}

/* Invalidate timer if any, and set new timer's interval to
   SECONDS.  */

- (void)rescheduleTimer:(NSTimeInterval)seconds
{
  [timer invalidate];

  if (seconds >= 0)
    {
      [timer release];
      timer = [[NSTimer scheduledTimerWithTimeInterval:seconds
			target:self selector:@selector(postMouseDraggedEvent:)
			userInfo:nil repeats:NO] retain];
    }
}

- (void)mouseDown:(NSEvent *)theEvent
{
  hitPart = [self testPart:[theEvent locationInWindow]];

  if (hitPart == NSScrollerNoPart)
    return;

  if (hitPart != NSScrollerKnob)
    {
      [self rescheduleTimer:[self firstDelay]];
      [self highlight:YES];
      [self sendAction:[self action] to:[self target]];
    }
  else
    {
      NSPoint point = [self convertPoint:[theEvent locationInWindow]
			    fromView:nil];
      NSRect frameRect, knobRect;

      frameRect = [self frame];
      knobRect = [self rectForPart:NSScrollerKnob];

      if (NSHeight (frameRect) >= NSWidth (frameRect))
	knobGrabOffset = - (point.y - NSMinY (knobRect)) - 1;
      else
	knobGrabOffset = - (point.x - NSMinX (knobRect)) - 1;
    }
}

- (void)mouseUp:(NSEvent *)theEvent
{
  NSScrollerPart lastPart = hitPart;

  [self highlight:NO];
  [self rescheduleTimer:-1];

  hitPart = NSScrollerNoPart;
  if (lastPart != NSScrollerKnob || knobGrabOffset >= 0)
    [self sendAction:[self action] to:[self target]];
}

- (void)rightMouseDown:(NSEvent *)theEvent
{
  [self mouseDown:theEvent];
}

- (void)rightMouseUp:(NSEvent *)theEvent
{
  [self mouseUp:theEvent];
}

- (void)otherMouseDown:(NSEvent *)theEvent
{
  [self mouseDown:theEvent];
}

- (void)otherMouseUp:(NSEvent *)theEvent
{
  [self mouseUp:theEvent];
}

- (void)mouseDragged:(NSEvent *)theEvent
{
  if (hitPart == NSScrollerNoPart)
    return;

  if (hitPart == NSScrollerKnob)
    {
      NSPoint point = [self convertPoint:[theEvent locationInWindow]
			    fromView:nil];
      NSRect frameRect, knobSlotRect;

      if (knobGrabOffset <= -1)
	knobGrabOffset = - (knobGrabOffset + 1);

      frameRect = [self frame];
      knobSlotRect = [self rectForPart:NSScrollerKnobSlot];
      if (NSHeight (frameRect) >= NSWidth (frameRect))
	knobMinEdgeInSlot = point.y - knobGrabOffset - NSMinY (knobSlotRect);
      else
	knobMinEdgeInSlot = point.x - knobGrabOffset - NSMinX (knobSlotRect);

      if ([self dragUpdatesFloatValue])
	{
	  CGFloat maximum, minEdge;
	  NSRect KnobRect = [self rectForPart:NSScrollerKnob];

	  if (NSHeight (frameRect) >= NSWidth (frameRect))
	    maximum = NSHeight (knobSlotRect) - NSHeight (KnobRect);
	  else
	    maximum = NSWidth (knobSlotRect) - NSWidth (KnobRect);

	  minEdge = knobMinEdgeInSlot;
	  if (minEdge < 0)
	    minEdge = 0;
	  if (minEdge > maximum)
	    minEdge = maximum;

	  [self setFloatValue:minEdge/maximum];
	}

      [self sendAction:[self action] to:[self target]];
    }
  else
    {
      BOOL unhilite = NO;
      NSScrollerPart part = [self testPart:[theEvent locationInWindow]];

      if (part == NSScrollerKnob)
	unhilite = YES;
      else
	{
	  switch (hitPart)
	    {
	    case NSScrollerIncrementPage:
	    case NSScrollerDecrementPage:
	      if (part != NSScrollerIncrementPage
		  && part != NSScrollerDecrementPage)
		unhilite = YES;
	      break;

	    case NSScrollerIncrementLine:
	    case NSScrollerDecrementLine:
	      if (part != NSScrollerIncrementLine
		  && part != NSScrollerDecrementLine)
		unhilite = YES;
	      break;
	    }
	}

      if (unhilite)
	[self highlight:NO];
      else if (part != hitPart || timer == nil)
	{
	  hitPart = part;
	  [self rescheduleTimer:[self continuousDelay]];
	  [self highlight:YES];
	  [self sendAction:[self action] to:[self target]];
	}
    }
}

@end				// NonmodalScroller

@implementation EmacsScroller

- (void)viewFrameDidChange:(NSNotification *)aNotification
{
  BOOL enabled = [self isEnabled], tooSmall = NO;
  float floatValue = [self floatValue];
  CGFloat knobProportion = [self knobProportion];
  NSRect frameRect, knobSlotRect, KnobRect;

  frameRect = [self frame];
  if (NSHeight (frameRect) >= NSWidth (frameRect))
    {
      if (NSWidth (frameRect) >= MAC_AQUA_VERTICAL_SCROLL_BAR_WIDTH)
	[self setControlSize:NSRegularControlSize];
      else if (NSWidth (frameRect) >= MAC_AQUA_SMALL_VERTICAL_SCROLL_BAR_WIDTH)
	[self setControlSize:NSSmallControlSize];
      else
	tooSmall = YES;
    }
  else
    {
      if (NSHeight (frameRect) >= MAC_AQUA_VERTICAL_SCROLL_BAR_WIDTH)
	[self setControlSize:NSRegularControlSize];
      else if (NSHeight (frameRect) >= MAC_AQUA_SMALL_VERTICAL_SCROLL_BAR_WIDTH)
	[self setControlSize:NSSmallControlSize];
      else
	tooSmall = YES;
    }

  [self setFloatValue:0 knobProportion:0];
  [self setEnabled:YES];
  knobSlotRect = [self rectForPart:NSScrollerKnobSlot];
  KnobRect = [self rectForPart:NSScrollerKnob];
  if (NSHeight (frameRect) >= NSWidth (frameRect))
    {
      knobSlotSpan = NSHeight (knobSlotRect);
      minKnobSpan = NSHeight (KnobRect);
    }
  else
    {
      knobSlotSpan = NSWidth (knobSlotRect);
      minKnobSpan = NSWidth (KnobRect);
    }

  if (!tooSmall)
    {
      [self setEnabled:enabled];
      [self setFloatValue:floatValue knobProportion:knobProportion];
    }
  else
    {
      [self setEnabled:NO];
      minKnobSpan = 0;
    }
}

- (id)initWithFrame:(NSRect)frameRect
{
  self = [super initWithFrame:frameRect];
  if (self == nil)
    return nil;

  [[NSNotificationCenter defaultCenter]
    addObserver:self
    selector:@selector(viewFrameDidChange:)
    name:@"NSViewFrameDidChangeNotification"
    object:self];

  [self viewFrameDidChange:nil];

  return self;
}

- (void)dealloc
{
  [[NSNotificationCenter defaultCenter] removeObserver:self];
  [super dealloc];
}

- (void)setEmacsScrollBar:(struct scroll_bar *)bar
{
  emacsScrollBar = bar;
}

- (struct scroll_bar *)emacsScrollBar
{
  return emacsScrollBar;
}

- (BOOL)dragUpdatesFloatValue
{
  return NO;
}

- (CGFloat)knobSlotSpan
{
  return knobSlotSpan;
}

- (CGFloat)minKnobSpan
{
  return minKnobSpan;
}

- (CGFloat)knobMinEdgeInSlot
{
  return knobMinEdgeInSlot;
}

- (CGFloat)frameSpan
{
  return frameSpan;
}

- (CGFloat)clickPositionInFrame
{
  return clickPositionInFrame;
}

- (int)inputEventModifiers
{
  return inputEventModifiers;
}

- (int)inputEventCode
{
  return inputEventCode;
}

- (void)mouseClick:(NSEvent *)theEvent;
{
  NSPoint point = [theEvent locationInWindow];
  NSRect frameRect = [self frame];

  hitPart = [self testPart:point];
  point = [self convertPoint:point fromView:nil];
  if (NSHeight (frameRect) >= NSWidth (frameRect))
    {
      frameSpan = NSHeight (frameRect);
      clickPositionInFrame = point.y;
    }
  else
    {
      frameSpan = NSWidth (frameRect);
      clickPositionInFrame = point.x;
    }
  inputEventCode = mac_get_mouse_btn (theEvent);
  [self sendAction:[self action] to:[self target]];
}

- (void)mouseDown:(NSEvent *)theEvent
{
  int modifiers = mac_event_to_emacs_modifiers (theEvent);

  last_mouse_glyph_frame = 0;

  /* Make the "Ctrl-Mouse-2 splits window" work for toolkit scroll bars.  */
  if (modifiers & ctrl_modifier)
    {
      inputEventModifiers = modifiers | down_modifier;
      [self mouseClick:theEvent];
    }
  else
    {
      inputEventModifiers = 0;
      [super mouseDown:theEvent];
    }
}

- (void)mouseDragged:(NSEvent *)theEvent
{
  if (inputEventModifiers == 0)
    [super mouseDragged:theEvent];
}

- (void)mouseUp:(NSEvent *)theEvent
{
  if (inputEventModifiers != 0)
    {
      int modifiers = mac_event_to_emacs_modifiers (theEvent);

      inputEventModifiers = modifiers | up_modifier;
      [self mouseClick:theEvent];
    }
  else
    [super mouseUp:theEvent];
}

@end				// EmacsScroller

@implementation EmacsView (ScrollBar)

static int
scroller_part_to_scroll_bar_part (part)
     NSScrollerPart part;
{
  switch (part)
    {
    case NSScrollerDecrementLine:	return scroll_bar_up_arrow;
    case NSScrollerIncrementLine:	return scroll_bar_down_arrow;
    case NSScrollerDecrementPage:	return scroll_bar_above_handle;
    case NSScrollerIncrementPage:	return scroll_bar_below_handle;
    case NSScrollerKnob:		return scroll_bar_handle;
    case NSScrollerNoPart:		return scroll_bar_end_scroll;
    }

  return -1;
}

/* Generate an Emacs input event in response to a scroller action sent
   from SENDER to the receiver Emacs view, and then send the action
   associated to the view to the target of the view.  */

- (void)convertScrollerAction:(id)sender
{
  struct scroll_bar *bar = [sender emacsScrollBar];
  NSScrollerPart hitPart = [sender hitPart];
  int modifiers = [sender inputEventModifiers];

  EVENT_INIT (inputEvent);
  inputEvent.arg = Qnil;
  inputEvent.kind = SCROLL_BAR_CLICK_EVENT;
  inputEvent.frame_or_window = bar->window;
  inputEvent.part = scroller_part_to_scroll_bar_part (hitPart);
  inputEvent.timestamp = [[NSApp currentEvent] timestamp] * 1000;
  inputEvent.modifiers = modifiers;

  if (modifiers)
    {
      CGFloat clickPositionInFrame = [sender clickPositionInFrame];
      CGFloat frameSpan = [sender frameSpan];
      int inputEventCode = [sender inputEventCode];

      if (clickPositionInFrame < 0)
	clickPositionInFrame = 0;
      if (clickPositionInFrame > frameSpan)
	clickPositionInFrame = frameSpan;

      XSETINT (inputEvent.x, clickPositionInFrame);
      XSETINT (inputEvent.y, frameSpan);
      if (inputEvent.part == scroll_bar_end_scroll)
	inputEvent.part = scroll_bar_handle;
      inputEvent.code = inputEventCode;
    }
  else if (hitPart == NSScrollerKnob)
    {
      CGFloat minEdge = [sender knobMinEdgeInSlot];
      CGFloat knobSlotSpan = [sender knobSlotSpan];
      CGFloat minKnobSpan = [sender minKnobSpan];
      CGFloat maximum = knobSlotSpan - minKnobSpan;

      if (minEdge < 0)
	minEdge = 0;
      if (minEdge > maximum)
	minEdge = maximum;

      XSETINT (inputEvent.x, minEdge);
      XSETINT (inputEvent.y, maximum);
    }

  [self sendAction:action to:target];
}

@end				// EmacsView (ScrollBar)

/* Create a scroll bar control for BAR.  BOUNDS and VISIBLE specifies
   the initial geometry and visibility, respectively.  The created
   control is stored in some members of BAR.  */

void
mac_create_scroll_bar (bar, bounds, visible)
     struct scroll_bar *bar;
     const Rect *bounds;
     Boolean visible;
{
  struct frame *f = XFRAME (WINDOW_FRAME (XWINDOW (bar->window)));
  NSRect frame = NSMakeRect (bounds->left, bounds->top,
			     bounds->right - bounds->left,
			     bounds->bottom - bounds->top);
  EmacsScroller *scroller = [[EmacsScroller alloc] initWithFrame:frame];
  EmacsView *emacsView = FRAME_EMACS_VIEW (f);

  [scroller setEmacsScrollBar:bar];
  [scroller setAction:@selector(convertScrollerAction:)];
  [emacsView addSubview:scroller];
  [scroller release];
  SET_SCROLL_BAR_SCROLLER (bar, scroller);
}

/* Dispose of the scroll bar control stored in some members of
   BAR.  */

void
mac_dispose_scroll_bar (bar)
     struct scroll_bar *bar;
{
  EmacsScroller *scroller = SCROLL_BAR_SCROLLER (bar);

  [scroller removeFromSuperview];
}

/* Set bounds of the scroll bar BAR to BOUNDS.  */

void
mac_set_scroll_bar_bounds (bar, bounds)
     struct scroll_bar *bar;
     const Rect *bounds;
{
  EmacsScroller *scroller = SCROLL_BAR_SCROLLER (bar);
  NSRect frame = NSMakeRect (bounds->left, bounds->top,
			     bounds->right - bounds->left,
			     bounds->bottom - bounds->top);

  [scroller setFrame:frame];
  [scroller setNeedsDisplay:YES];
}

/* Draw the scroll bar BAR.  */

void
mac_redraw_scroll_bar (bar)
     struct scroll_bar *bar;
{
  EmacsScroller *scroller = SCROLL_BAR_SCROLLER (bar);

  [scroller setNeedsDisplay:YES];
}

/* Set the thumb size and position of scroll bar BAR.  We are currently
   displaying PORTION out of a whole WHOLE, and our position POSITION.  */

void
x_set_toolkit_scroll_bar_thumb (bar, portion, position, whole)
     struct scroll_bar *bar;
     int portion, position, whole;
{
  EmacsScroller *scroller = SCROLL_BAR_SCROLLER (bar);
  CGFloat minKnobSpan;

  BLOCK_INPUT;

  /* Must be inside BLOCK_INPUT as objc_msgSend may call zone_free via
     _class_lookupMethodAndLoadCache, for example.  */
  minKnobSpan = [scroller minKnobSpan];
  if (minKnobSpan == 0)
    ;
  else if (whole <= portion)
    [scroller setEnabled:NO];
  else
    {
      CGFloat knobSlotSpan = [scroller knobSlotSpan];
      CGFloat maximum, scale, top, size;
      float floatValue;
      CGFloat knobProportion;

      maximum = knobSlotSpan - minKnobSpan;
      scale = maximum / whole;
      top = position * scale;
      size = portion * scale + minKnobSpan;

      floatValue = top / (knobSlotSpan - size);
      knobProportion = size / knobSlotSpan;

      [scroller setFloatValue:floatValue knobProportion:knobProportion];
      [scroller setEnabled:YES];
    }

  UNBLOCK_INPUT;
}


/***********************************************************************
			       Tool-bars
 ***********************************************************************/

#define TOOLBAR_IDENTIFIER_FORMAT (@"org.gnu.Emacs.%p.toolbar")

/* In identifiers such as function/variable names, Emacs tool bar is
   referred to as `tool_bar', and Carbon HIToolbar as `toolbar'.  */

#define TOOLBAR_ICON_ITEM_IDENTIFIER (@"org.gnu.Emacs.toolbar.icon")

extern void mac_move_window_with_gravity P_ ((struct frame *, int,
					      short, short));
extern void mac_get_window_origin_with_gravity P_ ((struct frame *, int,
						    short *, short *));
extern CGImageRef mac_image_spec_to_cg_image P_ ((struct frame *,
						  Lisp_Object));

@implementation EmacsToolbarItem

- (BOOL)allowsDuplicatesInToolbar
{
  return YES;
}

- (void)dealloc
{
  CGImageRelease (coreGraphicsImage);
  [super dealloc];
}

/* Set the toolbar icon image to the CoreGraphics image CGIMAGE.  */

- (void)setCoreGraphicsImage:(CGImageRef)cgImage
{
  if (coreGraphicsImage == cgImage)
    return;

  [self setImage:[NSImage imageWithCGImage:cgImage]];
  CGImageRelease (coreGraphicsImage);
  coreGraphicsImage = CGImageRetain (cgImage);
}

- (void)setImage:(NSImage *)image
{
  [super setImage:image];
  CGImageRelease (coreGraphicsImage);
  coreGraphicsImage = nil;
}

@end				// EmacsToolbarItem

@implementation EmacsFrameController (Toolbar)

- (NSToolbarItem *)toolbar:(NSToolbar *)toolbar
     itemForItemIdentifier:(NSString *)itemIdentifier
 willBeInsertedIntoToolbar:(BOOL)flag
{
  NSToolbarItem *item = nil;

  if ([itemIdentifier isEqualToString:TOOLBAR_ICON_ITEM_IDENTIFIER])
    {
      item = [[[EmacsToolbarItem alloc] initWithItemIdentifier:itemIdentifier]
	       autorelease];
      [item setTarget:self];
      [item setAction:@selector(storeToolBarEvent:)];
    }

  return item;
}

- (NSArray *)toolbarAllowedItemIdentifiers:(NSToolbar *)toolbar
{
  return [NSArray arrayWithObject:TOOLBAR_ICON_ITEM_IDENTIFIER];
}

- (NSArray *)toolbarDefaultItemIdentifiers:(NSToolbar *)toolbar
{
  return [NSArray array];
}

- (BOOL)validateToolbarItem:(NSToolbarItem *)theItem
{
  return [theItem isEnabled];
}

/* Store toolbar item click event from SENDER to kbd_buffer.  */

- (void)storeToolBarEvent:(id)sender
{
  NSInteger i = [sender tag];
  struct frame *f = emacsFrame;

#define PROP(IDX) AREF (f->tool_bar_items, i * TOOL_BAR_ITEM_NSLOTS + (IDX))
  if (i < f->n_tool_bar_items && !NILP (PROP (TOOL_BAR_ITEM_ENABLED_P)))
    {
      Lisp_Object frame;
      struct input_event buf;

      EVENT_INIT (buf);

      XSETFRAME (frame, f);
      buf.kind = TOOL_BAR_EVENT;
      buf.frame_or_window = frame;
      buf.arg = frame;
      kbd_buffer_store_event (&buf);

      buf.kind = TOOL_BAR_EVENT;
      buf.frame_or_window = frame;
      buf.arg = PROP (TOOL_BAR_ITEM_KEY);
      buf.modifiers = mac_event_to_emacs_modifiers ([NSApp currentEvent]);
      kbd_buffer_store_event (&buf);
    }
#undef PROP
}

@end				// EmacsFrameController (Toolbar)

/* Whether the toolbar for the window WINDOW is visible.  */

Boolean
mac_is_window_toolbar_visible (window)
     Window window;
{
  NSToolbar *toolbar = [(NSWindow *)window toolbar];

  return [toolbar isVisible];
}

/* Update the tool bar for frame F.  Add new buttons and remove old.  */

void
update_frame_tool_bar (f)
     FRAME_PTR f;
{
  NSWindow *window = FRAME_MAC_WINDOW (f);
  short left, top;
  Rect inner, outer;
  NSToolbar *toolbar;
  NSArray *items;
  NSUInteger count;
  int i, pos, win_gravity = f->output_data.mac->toolbar_win_gravity;

  BLOCK_INPUT;

  if (win_gravity >= NorthWestGravity && win_gravity <= SouthEastGravity)
    mac_get_window_origin_with_gravity (f, win_gravity, &left, &top);

  mac_get_window_bounds (f, &inner, &outer);

  toolbar = [window toolbar];
  items = [toolbar items];
  count = [items count];
  pos = 0;
  for (i = 0; i < f->n_tool_bar_items; ++i)
    {
#define PROP(IDX) AREF (f->tool_bar_items, i * TOOL_BAR_ITEM_NSLOTS + (IDX))
      int enabled_p = !NILP (PROP (TOOL_BAR_ITEM_ENABLED_P));
      int selected_p = !NILP (PROP (TOOL_BAR_ITEM_SELECTED_P));
      int idx;
      Lisp_Object image;
      CGImageRef cg_image;
      NSString *label, *identifier = TOOLBAR_ICON_ITEM_IDENTIFIER;

      /* If image is a vector, choose the image according to the
	 button state.  */
      image = PROP (TOOL_BAR_ITEM_IMAGES);
      if (VECTORP (image))
	{
	  if (enabled_p)
	    idx = (selected_p
		   ? TOOL_BAR_IMAGE_ENABLED_SELECTED
		   : TOOL_BAR_IMAGE_ENABLED_DESELECTED);
	  else
	    idx = (selected_p
		   ? TOOL_BAR_IMAGE_DISABLED_SELECTED
		   : TOOL_BAR_IMAGE_DISABLED_DESELECTED);

	  xassert (ASIZE (image) >= idx);
	  image = AREF (image, idx);
	}
      else
	idx = -1;

      cg_image = mac_image_spec_to_cg_image (f, image);
      /* Ignore invalid image specifications.  */
      if (cg_image == NULL)
	continue;

      label = [NSString stringWithLispString:(PROP (TOOL_BAR_ITEM_CAPTION))];
      if (label == nil)
	label = @"";

      /* As displayed images of toolbar image items are scaled to
	 square shapes, narrow images such as separators look weird.
	 So we use separator items for too narrow disabled images.  */
      if (CGImageGetWidth (cg_image) <= 2 && !enabled_p)
	identifier = NSToolbarSeparatorItemIdentifier;

      if (pos >= count
	  || ![identifier isEqualToString:[[items objectAtIndex:pos]
					    itemIdentifier]])
	{
	  [toolbar insertItemWithItemIdentifier:identifier atIndex:pos];
	  items = [toolbar items];
	  count = [items count];
	}

      if (identifier != NSToolbarSeparatorItemIdentifier)
	{
	  EmacsToolbarItem *item = [items objectAtIndex:pos];

	  [item setCoreGraphicsImage:cg_image];
	  [item setLabel:label];
	  [item setEnabled:(enabled_p || idx >= 0)];
	  [item setTag:i];
	}
      pos++;
#undef PROP
    }

#if 0
  /* This leads to the problem that the toolbar space right to the
     icons cannot be dragged if it becomes wider on Mac OS X 10.5. */
  while (pos < count)
    [toolbar removeItemAtIndex:--count];
#else
  while (pos < count)
    {
      [toolbar removeItemAtIndex:pos];
      count--;
    }
#endif

  UNBLOCK_INPUT;

  /* Check if the window has moved during toolbar item setup.  As
     title bar dragging is processed asynchronously, we don't
     notice it without reading window events.  */
  if (input_polling_used ())
    {
      /* It could be confusing if a real alarm arrives while
	 processing the fake one.  Turn it off and let the handler
	 reset it.  */
      extern void poll_for_input_1 P_ ((void));
      int old_poll_suppress_count = poll_suppress_count;
      poll_suppress_count = 1;
      poll_for_input_1 ();
      poll_suppress_count = old_poll_suppress_count;
    }

  BLOCK_INPUT;

  /* If we change the visibility of a toolbar while its window is
     being moved asynchronously, the window moves to the original
     position.  How can we know we are in asynchronous dragging?  Note
     that sometimes we don't receive windowDidMove: messages for
     preceding windowWillMove:.  */
  [toolbar setVisible:YES];

  win_gravity = f->output_data.mac->toolbar_win_gravity;
  if (win_gravity >= NorthWestGravity && win_gravity <= SouthEastGravity)
    {
      /* This is a workaround for Mac OS X 10.3 or earlier.  Without
         this, the toolbar may not be shown if the height of the
         visible frame of the screen is not enough for the new window.  */
      [window displayIfNeeded];

      mac_get_window_bounds (f, &inner, &outer);

      mac_move_window_with_gravity (f, win_gravity, left, top);
    }
  f->output_data.mac->toolbar_win_gravity = 0;

  UNBLOCK_INPUT;
}

/* Hide the tool bar on frame F.  Unlike the counterpart on GTK+, it
   doesn't deallocate the resources.  */

void
free_frame_tool_bar (f)
     FRAME_PTR f;
{
  NSWindow *window = FRAME_MAC_WINDOW (f);
  NSToolbar *toolbar;

  BLOCK_INPUT;

  toolbar = [window toolbar];
  if ([toolbar isVisible])
    [toolbar setVisible:NO];

  UNBLOCK_INPUT;
}

/* Report a mouse movement over toolbar to the mainstream Emacs
   code.  */

static void
mac_tool_bar_note_mouse_movement (f, event)
     struct frame *f;
     NSEvent *event;
{
  NSWindow *window;
  NSView *hitView;

  /* Return if mouse dragged.  */
  if ([event type] != NSMouseMoved)
    return;

  window = FRAME_MAC_WINDOW (f);
  hitView = [[[window contentView] superview] hitTest:[event locationInWindow]];
  if ([hitView respondsToSelector:@selector(item)])
    {
      id item = [hitView performSelector:@selector(item)];

      if ([item isKindOfClass:[EmacsToolbarItem class]])
	{
#define PROP(IDX) AREF (f->tool_bar_items, i * TOOL_BAR_ITEM_NSLOTS + (IDX))
	  NSInteger i = [item tag];

	  if (i < f->n_tool_bar_items)
	    {
	      NSRect viewFrame = [hitView frame];
	      EmacsView *emacsView = FRAME_EMACS_VIEW (f);

	      viewFrame = [hitView convertRect:viewFrame toView:nil];
	      viewFrame = [emacsView convertRect:viewFrame fromView:nil];
	      SetRect (&last_mouse_glyph,
		       NSMinX (viewFrame), NSMinY (viewFrame),
		       NSMaxX (viewFrame), NSMaxY (viewFrame));

	      help_echo_object = help_echo_window = Qnil;
	      help_echo_pos = -1;
	      help_echo_string = PROP (TOOL_BAR_ITEM_HELP);
	      if (NILP (help_echo_string))
		help_echo_string = PROP (TOOL_BAR_ITEM_CAPTION);
	    }
	}
    }
#undef PROP
}

/* Create a tool bar for frame F.  */

static OSStatus
mac_create_frame_tool_bar (f)
     FRAME_PTR f;
{
  NSString *identifier =
    [NSString stringWithFormat:TOOLBAR_IDENTIFIER_FORMAT, f];
  NSToolbar *toolbar = [[NSToolbar alloc] initWithIdentifier:identifier];
  NSWindow *window = FRAME_MAC_WINDOW (f);
  NSButton *button;

  if (toolbar == nil)
    return memFullErr;

  [toolbar setDisplayMode:NSToolbarDisplayModeIconOnly];
  [toolbar setSizeMode:NSToolbarSizeModeSmall];
  [toolbar setAllowsUserCustomization:NO];
  [toolbar setAutosavesConfiguration:NO];
  [toolbar setDelegate:[window delegate]];
  [toolbar setVisible:NO];

  [window setToolbar:toolbar];
  [toolbar release];

  button = [window standardWindowButton:NSWindowToolbarButton];
  [button setTarget:[NSApp delegate]];
  [button setAction:(NSSelectorFromString (@"toolbar-pill-button-clicked:"))];

  return noErr;
}


/***********************************************************************
			      Font Panel
 ***********************************************************************/

extern Lisp_Object Qpanel_closed, Qselection;
extern Lisp_Object Qfont_spec;

extern OSStatus mac_store_event_ref_as_apple_event P_ ((AEEventClass, AEEventID,
							Lisp_Object,
							Lisp_Object,
							EventRef, UInt32,
							const EventParamName *,
							const EventParamType *));

#if MAC_OS_X_VERSION_MAX_ALLOWED < 1030
/* Font panel mode masks used as a return value of validModesForFontPanel:. */
enum {
  NSFontPanelFaceModeMask = 1 << 0,
  NSFontPanelSizeModeMask = 1 << 1,
  NSFontPanelCollectionModeMask = 1 << 2,
};
#endif

@implementation EmacsFontPanel

- (void)suspendSliderTracking:(NSEvent *)event
{
  mouseUpEvent = [[event mouseEventByChangingType:NSLeftMouseUp
			 andLocation:[event locationInWindow]] retain];
  [NSApp postEvent:mouseUpEvent atStart:YES];
  /* Use notification?  */
  [[NSApp delegate] setTrackingObject:self
		    andResumeSelector:@selector(resumeSliderTracking)];
}

- (void)resumeSliderTracking
{
  NSPoint location = [mouseUpEvent locationInWindow];
  NSRect trackRect;
  NSEvent *mouseDownEvent;

  trackRect = [trackedSlider convertRect:[[trackedSlider cell] trackRect]
			     toView:nil];
  if (location.x < NSMinX (trackRect))
    location.x = NSMinX (trackRect);
  else if (location.x >= NSMaxX (trackRect))
    location.x = NSMaxX (trackRect) - 1;
  if (location.y <= NSMinY (trackRect))
    location.y = NSMinY (trackRect) + 1;
  else if (location.y > NSMaxY (trackRect))
    location.y = NSMaxY (trackRect);

  mouseDownEvent = [mouseUpEvent mouseEventByChangingType:NSLeftMouseDown
				 andLocation:location];
  [mouseUpEvent release];
  mouseUpEvent = nil;
  [NSApp postEvent:mouseDownEvent atStart:YES];
}

- (void)sendEvent:(NSEvent *)event
{
  if ([event type] == NSLeftMouseDown)
    {
      NSView *contentView = [self contentView], *hitView;

      hitView = [contentView hitTest:[[contentView superview]
				       convertPoint:[event locationInWindow]
				       fromView:nil]];
      if ([hitView isKindOfClass:[NSSlider class]])
	trackedSlider = (NSSlider *) hitView;
    }

  [super sendEvent:event];
}

@end				// EmacsFontPanel

@implementation EmacsController (FontPanel)

/* Called when the font panel is about to close.  */

- (void)fontPanelWillClose:(NSNotification *)aNotification
{
  OSStatus err;
  EventRef event;

  err = CreateEvent (NULL, kEventClassFont, kEventFontPanelClosed, 0,
		     kEventAttributeNone, &event);
  if (err == noErr)
    {
      err = mac_store_event_ref_as_apple_event (0, 0, Qfont, Qpanel_closed,
						event, 0, NULL, NULL);
      ReleaseEvent (event);
    }
}

@end				// EmacsController (FontPanel)

@implementation EmacsFrameController (FontPanel)

/* Return the NSFont object for the face FACEID and the character C.  */

- (NSFont *)fontForFace:(int)faceId character:(int)c
	       position:(int)pos object:(Lisp_Object)object
{
  struct frame *f = emacsFrame;

  if (FRAME_FACE_CACHE (f) && CHAR_VALID_P (c, 0))
    {
      struct face *face;

      faceId = FACE_FOR_CHAR (f, FACE_FROM_ID (f, faceId), c, pos, object);
      face = FACE_FROM_ID (f, faceId);

      return [NSFont fontWithFace:face];
    }
  else
    return nil;
}

/* Called when the user has chosen a font from the font panel.  */

- (void)changeFont:(id)sender
{
  NSEvent *currentEvent = [NSApp currentEvent];
  NSFont *oldFont = [self fontForFace:DEFAULT_FACE_ID character:0
			  position:-1 object:Qnil];
  NSFont *newFont = [sender convertFont:oldFont];
  Lisp_Object arg = Qnil;
  struct input_event inev;

  if ([currentEvent type] == NSLeftMouseDragged)
    {
      EmacsFontPanel *fontPanel = (EmacsFontPanel *) [sender fontPanel:NO];

      [fontPanel suspendSliderTracking:currentEvent];
    }

  if (newFont)
    arg = Fcons (Fcons (Qfont_spec,
			Fcons (build_string ("Lisp"),
			       macfont_nsctfont_to_spec (newFont))),
		 arg);

  EVENT_INIT (inev);
  inev.kind = MAC_APPLE_EVENT;
  inev.x = Qfont;
  inev.y = Qselection;
  XSETFRAME (inev.frame_or_window,
	     mac_focus_frame (&one_mac_display_info));
  inev.arg = Fcons (build_string ("aevt"), arg);
  [[NSApp delegate] storeEvent:&inev];
}

/* Hide unused features in font panels.  */

- (NSUInteger)validModesForFontPanel:(NSFontPanel *)fontPanel
{
  /* Underline, Strikethrough, TextColor, DocumentColor, and Shadow
     are not used in font panels.  */
  return (NSFontPanelFaceModeMask
	  | NSFontPanelSizeModeMask
	  | NSFontPanelCollectionModeMask);
}

@end				// EmacsFrameController (FontPanel)

/* Whether the font panel is currently visible.  */

int
mac_font_panel_visible_p ()
{
  NSFontPanel *fontPanel = [[NSFontManager sharedFontManager] fontPanel:NO];

  return [fontPanel isVisible];
}

/* Toggle visiblity of the font panel.  */

OSStatus
mac_show_hide_font_panel ()
{
  static BOOL initialized;
  NSFontManager *fontManager = [NSFontManager sharedFontManager];
  NSFontPanel *fontPanel = [fontManager fontPanel:YES];

  if (!initialized)
    {
      [[NSNotificationCenter defaultCenter]
	addObserver:[NSApp delegate]
	selector:@selector(fontPanelWillClose:)
	name:@"NSWindowWillCloseNotification"
	object:fontPanel];
      initialized = YES;
    }

  if ([fontPanel isVisible])
    [fontPanel orderOut:nil];
  else
    [fontManager orderFrontFontPanel:nil];

  return noErr;
}

/* Set the font selected in the font panel to the one corresponding to
   the face FACE_ID and the charcacter C in the frame F.  */

OSStatus
mac_set_font_info_for_selection (f, face_id, c, pos, object)
     struct frame *f;
     int face_id, c, pos;
     Lisp_Object object;
{
  if (mac_font_panel_visible_p () && f)
    {
      NSWindow *window = FRAME_MAC_WINDOW (f);
      NSFont *font = [[window delegate] fontForFace:face_id character:c
					position:pos object:object];

      [[NSFontManager sharedFontManager] setSelectedFont:font isMultiple:NO];
    }

  return noErr;
}


/************************************************************************
			    Event Handling
 ************************************************************************/

extern void mac_get_screen_info P_ ((struct mac_display_info *));

static void update_apple_event_handler P_ ((void));
static void update_dragged_types P_ ((void));

/* Specify how long dpyinfo->saved_menu_event remains valid in
   seconds.  This is to avoid infinitely ignoring mouse events when
   MENU_BAR_ACTIVATE_EVENT is not processed: e.g., "M-! sleep 30 RET
   -> try to activate menu bar -> C-g".  */
#define SAVE_MENU_EVENT_TIMEOUT	5

/* Minimum time interval between successive XTread_socket calls.  */
#define READ_SOCKET_MIN_INTERVAL (1/60.0)

/* Convert Cocoa modifier key masks to Carbon key modifiers.  */

static UInt32
mac_modifier_flags_to_modifiers (flags)
    NSUInteger flags;
{
  UInt32 modifiers = 0;

  if (flags & NSAlphaShiftKeyMask)
    modifiers |= alphaLock;
  if (flags & NSShiftKeyMask)
    modifiers |= shiftKey;
  if (flags & NSControlKeyMask)
    modifiers |= controlKey;
  if (flags & NSAlternateKeyMask)
    modifiers |= optionKey;
  if (flags & NSCommandKeyMask)
    modifiers |= cmdKey;
  if (flags & NSNumericPadKeyMask)
    modifiers |= kEventKeyModifierNumLockMask;
  /* if (flags & NSHelpKeyMask); */
  if (flags & NSFunctionKeyMask)
    modifiers |= kEventKeyModifierFnMask;

  return modifiers;
}

/* Given an EVENT, return the code to use for the mouse button code in
   the emacs input_event.  */

static int
mac_get_mouse_btn (event)
     NSEvent *event;
{
  NSInteger button_number = [event buttonNumber];

  switch (button_number)
    {
    case 0:
      if (NILP (Vmac_emulate_three_button_mouse))
	return 0;
      else
	{
	  NSUInteger flags = [event modifierFlags];
	  UInt32 modifiers = mac_modifier_flags_to_modifiers (flags);

	  return mac_get_emulated_btn (modifiers);
	}
    case 1:
      return mac_wheel_button_is_mouse_2 ? 2 : 1;
    case 2:
      return mac_wheel_button_is_mouse_2 ? 1 : 2;
    default:
      return button_number;
    }
}

/* Obtains the event modifiers from the event EVENT and then calls
   mac_to_emacs_modifiers.  */

static int
mac_event_to_emacs_modifiers (event)
     NSEvent *event;
{
  NSUInteger flags = [event modifierFlags];
  UInt32 modifiers = mac_modifier_flags_to_modifiers (flags);

  int mouse_event_p = (NSEventMaskFromType ([event type])
		       & ANY_MOUSE_EVENT_MASK);

  if (!NILP (Vmac_emulate_three_button_mouse) && mouse_event_p)
    modifiers &= ~(optionKey | cmdKey);

  return mac_to_emacs_modifiers (modifiers, 0);
}

/* Function to report a mouse movement to the mainstream Emacs code.
   The input handler calls this.

   We have received a mouse movement event, which is given in *event.
   If the mouse is over a different glyph than it was last time, tell
   the mainstream emacs code by setting mouse_moved.  If not, ask for
   another motion event, so we can check again the next time it moves.  */

static int
note_mouse_movement (frame, point)
     FRAME_PTR frame;
     NSPoint point;
{
  struct mac_display_info *dpyinfo = FRAME_MAC_DISPLAY_INFO (frame);
  EmacsView *emacsView = FRAME_EMACS_VIEW (frame);
  NSRect emacsViewFrame = [emacsView frame];
  int x, y;

  last_mouse_movement_time = TickCount () * (1000 / 60);  /* to milliseconds */

  if (frame == dpyinfo->mouse_face_mouse_frame
      && ! (point.x >= 0 && point.x < NSMaxX (emacsViewFrame)
	    && point.y >= 0 && point.y < NSMaxY (emacsViewFrame)))
    {
      /* This case corresponds to LeaveNotify in X11.  If we move
	 outside the frame, then we're certainly no longer on any text
	 in the frame.  */
      clear_mouse_face (dpyinfo);
      dpyinfo->mouse_face_mouse_frame = 0;
      if (!dpyinfo->grabbed)
	{
	  struct redisplay_interface *rif = FRAME_RIF (frame);

	  rif->define_frame_cursor (frame,
				    frame->output_data.mac->nontext_cursor);
	}
    }

  x = point.x;
  y = point.y;

  /* Has the mouse moved off the glyph it was on at the last sighting?  */
  if (frame != last_mouse_glyph_frame
      || x < last_mouse_glyph.left
      || x >= last_mouse_glyph.right
      || y < last_mouse_glyph.top
      || y >= last_mouse_glyph.bottom)
    {
      EmacsView *emacsView = FRAME_EMACS_VIEW (frame);

      frame->mouse_moved = 1;
      [emacsView lockFocus];
      set_global_focus_view_frame (frame);
      note_mouse_highlight (frame, x, y);
      unset_global_focus_view_frame ();
      [emacsView unlockFocus];
      /* Remember which glyph we're now on.  */
      remember_mouse_glyph (frame, x, y, &last_mouse_glyph);
      last_mouse_glyph_frame = frame;
      return 1;
    }

  return 0;
}

/* Run the current run loop in the default mode until some input
   happens or TIMEOUT seconds passes unless it is negative.  Return
   true if timeout occurs first.  */

Boolean
mac_run_loop_run_once (timeout)
     EventTimeout timeout;
{
  NSDate *expiration;

  if (timeout < 0)
    expiration = [NSDate distantFuture];
  else
    expiration = [NSDate dateWithTimeIntervalSinceNow:timeout];

  [[NSRunLoop currentRunLoop] runMode:NSDefaultRunLoopMode
			      beforeDate:expiration];
  return [expiration timeIntervalSinceNow] <= 0;
}

/* Return next event in the main queue if it exists.  Otherwise return
   NULL.  */

static EventRef
peek_next_event ()
{
  EventRef event;

#if MAC_OS_X_VERSION_MAX_ALLOWED >= 1030
#if MAC_OS_X_VERSION_MIN_REQUIRED == 1020
  if (AcquireFirstMatchingEventInQueue != NULL)
#endif
    {
      event = AcquireFirstMatchingEventInQueue (GetCurrentEventQueue (), 0,
						NULL, kEventQueueOptionsNone);
      if (event)
	ReleaseEvent (event);
    }
#if MAC_OS_X_VERSION_MIN_REQUIRED == 1020
  else			/* AcquireFirstMatchingEventInQueue == NULL */
#endif
#endif	/* MAC_OS_X_VERSION_MAX_ALLOWED >= 1030  */
#if MAC_OS_X_VERSION_MAX_ALLOWED < 1030 || MAC_OS_X_VERSION_MIN_REQUIRED == 1020
    {
      OSStatus err;

      err = ReceiveNextEvent (0, NULL, kEventDurationNoWait,
			      kEventLeaveInQueue, &event);
      if (err != noErr)
	event = NULL;
    }
#endif

  return event;
}

/* Return next event in the main queue if it exists and is a mouse
   down on the menu bar.  Otherwise return NULL.  */

static EventRef
peek_if_next_event_activates_menu_bar ()
{
  EventRef event = peek_next_event ();

  if (event
      && GetEventClass (event) == kEventClassMouse
      && GetEventKind (event) == kEventMouseDown)
    {
      OSStatus err;
      HIPoint point;

      err = GetEventParameter (event, kEventParamMouseLocation,
			       typeHIPoint, NULL, sizeof (HIPoint), NULL,
			       &point);
      if (err == noErr
	  && point.x >= 0 && point.y >= 0
	  && point.x < CGDisplayPixelsWide (kCGDirectMainDisplay))
	{
	  CGFloat menuBarHeight;

#if MAC_OS_X_VERSION_MAX_ALLOWED >= 1050
#if MAC_OS_X_VERSION_MIN_REQUIRED < 1050 && MAC_OS_X_VERSION_MIN_REQUIRED >= 1020
	  /* -[NSMenu menuBarHeight] is unreliable on 10.4. */
	  if (!(floor (NSAppKitVersionNumber) <= NSAppKitVersionNumber10_4))
#endif
	    {
	      menuBarHeight = [[NSApp mainMenu] menuBarHeight];
	    }
#if MAC_OS_X_VERSION_MIN_REQUIRED < 1050 && MAC_OS_X_VERSION_MIN_REQUIRED >= 1020
	  else
#endif
#endif
#if MAC_OS_X_VERSION_MAX_ALLOWED < 1050 || (MAC_OS_X_VERSION_MIN_REQUIRED < 1050 && MAC_OS_X_VERSION_MIN_REQUIRED >= 1020)
	    {
	      menuBarHeight = [NSMenuView menuBarHeight];
	    }
#endif

	  if (point.y < menuBarHeight)
	    return event;
	}
    }

  return NULL;
}

/* Emacs calls this whenever it wants to read an input event from the
   user. */

int
XTread_socket (terminal, expected, hold_quit)
     struct terminal *terminal;
     int expected;
     struct input_event *hold_quit;
{
  int count;
  struct mac_display_info *dpyinfo = &one_mac_display_info;
  NSAutoreleasePool *pool;
  static NSDate *lastCallDate;
  static NSTimer *timer;
  NSTimeInterval timeInterval;

  if (interrupt_input_blocked)
    {
      interrupt_input_pending = 1;
#ifdef SYNC_INPUT
      pending_signals = 1;
#endif
      return -1;
    }

  interrupt_input_pending = 0;
#ifdef SYNC_INPUT
  pending_signals = pending_atimers;
#endif
  BLOCK_INPUT;

  /* So people can tell when we have read the available input.  */
  input_signal_count++;

  ++handling_signal;

  pool = [[NSAutoreleasePool alloc] init];

  if (lastCallDate
      && (timeInterval = - [lastCallDate timeIntervalSinceNow],
	  timeInterval < READ_SOCKET_MIN_INTERVAL)
      && !emacs_windows_need_display_p (0))
    {
      if (![timer isValid])
	{
	  [timer release];
	  timeInterval = READ_SOCKET_MIN_INTERVAL - timeInterval;
	  timer = [[NSTimer scheduledTimerWithTimeInterval:timeInterval
			    target:[NSApp delegate]
			    selector:@selector(processDeferredReadSocket:)
			    userInfo:nil repeats:NO] retain];
	}
      count = 0;
    }
  else
    {
      Lisp_Object tail, frame;

      [lastCallDate release];
      lastCallDate = [[NSDate alloc] init];
      [timer invalidate];
      [timer release];
      timer = nil;

      /* Maybe these should be done at some redisplay timing.  */
      update_apple_event_handler ();
      update_dragged_types ();

      if (dpyinfo->saved_menu_event
	  && (GetEventTime (dpyinfo->saved_menu_event) + SAVE_MENU_EVENT_TIMEOUT
	      <= GetCurrentEventTime ()))
	{
	  ReleaseEvent (dpyinfo->saved_menu_event);
	  dpyinfo->saved_menu_event = NULL;
	}

      count =
	[[NSApp delegate] handleQueuedNSEventsWithHoldingQuitIn:hold_quit];

      /* If the focus was just given to an autoraising frame,
	 raise it now.  */
      /* ??? This ought to be able to handle more than one such frame.  */
      if (pending_autoraise_frame)
	{
	  x_raise_frame (pending_autoraise_frame);
	  pending_autoraise_frame = 0;
	}

      if (mac_screen_config_changed)
	{
	  mac_get_screen_info (dpyinfo);
	  mac_screen_config_changed = 0;
	}

      FOR_EACH_FRAME (tail, frame)
	{
	  struct frame *f = XFRAME (frame);

	  /* The tooltip has been drawn already.  Avoid the
	     SET_FRAME_GARBAGED in mac_handle_visibility_change.  */
	  if (EQ (frame, tip_frame))
	    {
	      x_flush (f);
	      continue;
	    }

	  if (FRAME_MAC_P (f))
	    {
	      EmacsWindow *window = FRAME_MAC_WINDOW (f);

	      [window displayResizeControlIfNeeded];
	      x_flush (f);
#if MAC_OS_X_VERSION_MIN_REQUIRED < 1050
	      /* Mac OS X 10.4 seems not to reset the flag
		 `viewsNeedDisplay' on autodisplay.  */
	      if (floor (NSAppKitVersionNumber) <= NSAppKitVersionNumber10_4)
		[window setViewsNeedDisplay:NO];
#endif
	      /* Check which frames are still visible.  We do this
		 here because there doesn't seem to be any direct
		 notification that the visibility of a window has
		 changed (at least, not in all cases.  Or are there
		 any counterparts of kEventWindowShown/Hidden?).  */
	      mac_handle_visibility_change (f);
	    }
	}
    }

  [pool release];

  --handling_signal;
  UNBLOCK_INPUT;

  return count;
}


/***********************************************************************
				Busy cursor
 ***********************************************************************/

/* Show the spinning progress indicator for the frame F.  Create it if
   it doesn't exist yet. */

void
mac_show_hourglass (f)
     struct frame *f;
{
  NSProgressIndicator *indicator =
    (NSProgressIndicator *) f->output_data.mac->hourglass_control;

  if (indicator == nil)
    {
      NSWindow *window = FRAME_MAC_WINDOW (f);
      NSView *frameView = [[window contentView] superview];
      NSRect frameViewFrame, indicatorFrame;

      frameViewFrame = [frameView frame];
      indicatorFrame = NSMakeRect (NSWidth (frameViewFrame)
				   - (HOURGLASS_WIDTH + HOURGLASS_RIGHT_MARGIN),
				   NSHeight (frameViewFrame)
				   - (HOURGLASS_HEIGHT + HOURGLASS_TOP_MARGIN),
				   HOURGLASS_WIDTH, HOURGLASS_HEIGHT);
      indicator = [[NSProgressIndicator alloc] initWithFrame:indicatorFrame];
      [indicator setStyle:NSProgressIndicatorSpinningStyle];
      [indicator setDisplayedWhenStopped:NO];
      f->output_data.mac->hourglass_control = indicator;
      [frameView addSubview:indicator];
      [indicator setAutoresizingMask:(NSViewMinXMargin | NSViewMinYMargin)];
      [indicator release];
    }

  [indicator startAnimation:nil];
}

/* Hide the spinning progress indicator for the frame F.  Do nothing
   it doesn't exist yet. */

void
mac_hide_hourglass (f)
     struct frame *f;
{
  NSProgressIndicator *indicator =
    (NSProgressIndicator *) f->output_data.mac->hourglass_control;

  [indicator stopAnimation:nil];
}

/* Reposition the spinning progress indicator for the frame F.  Do
   nothing it doesn't exist yet. */

void
mac_reposition_hourglass (f)
     struct frame *f;
{
  /* Nothing to do.  */
}


/***********************************************************************
			File selection dialog
 ***********************************************************************/

@implementation EmacsSavePanel

/* Like the original runModal, but run the application event loop if
   not.  */

- (NSInteger)runModal
{
  if ([NSApp isRunning])
    return [super runModal];
  else
    {
      NSMethodSignature *signature = [self methodSignatureForSelector:_cmd];
      NSInvocation *invocation =
	[NSInvocation invocationWithMethodSignature:signature];
      NSInteger response;

      [invocation setTarget:self];
      [invocation setSelector:_cmd];

      [NSApp runTemporarilyWithInvocation:invocation];

      [invocation getReturnValue:&response];

      return response;
    }
}

/* Like the original runModalForDirectory:file:, but run the
   application event loop if not.  */

- (NSInteger)runModalForDirectory:(NSString *)path file:(NSString *)filename
{
  if ([NSApp isRunning])
    return [super runModalForDirectory:path file:filename];
  else
    {
      NSMethodSignature *signature = [self methodSignatureForSelector:_cmd];
      NSInvocation *invocation =
	[NSInvocation invocationWithMethodSignature:signature];
      NSInteger response;

      [invocation setTarget:self];
      [invocation setSelector:_cmd];
      [invocation setArgument:&path atIndex:2];
      [invocation setArgument:&filename atIndex:3];

      [NSApp runTemporarilyWithInvocation:invocation];

      [invocation getReturnValue:&response];

      return response;
    }
}

/* Simulate kNavDontConfirmReplacement.  */

- (BOOL)_overwriteExistingFileCheck:(id)fp8
{
  return YES;
}

@end				// EmacsSavePanel

@implementation EmacsOpenPanel

/* Like the original runModalForTypes:, but run the application event
   loop if not.  */

- (NSInteger)runModalForTypes:(NSArray *)fileTypes
{
  if ([NSApp isRunning])
    return [super runModalForTypes:fileTypes];
  else
    {
      NSMethodSignature *signature = [self methodSignatureForSelector:_cmd];
      NSInvocation *invocation =
	[NSInvocation invocationWithMethodSignature:signature];
      NSInteger response;

      [invocation setTarget:self];
      [invocation setSelector:_cmd];
      [invocation setArgument:&fileTypes atIndex:2];

      [NSApp runTemporarilyWithInvocation:invocation];

      [invocation getReturnValue:&response];

      return response;
    }
}

/* Like the original runModalForDirectory:file:types:, but run the
   application event loop if not.  */

- (NSInteger)runModalForDirectory:(NSString *)absoluteDirectoryPath
			     file:(NSString *)filename
			    types:(NSArray *)fileTypes
{
  if ([NSApp isRunning])
    return [super runModalForDirectory:absoluteDirectoryPath
		  file:filename types:fileTypes];
  else
    {
      NSMethodSignature *signature = [self methodSignatureForSelector:_cmd];
      NSInvocation *invocation =
	[NSInvocation invocationWithMethodSignature:signature];
      NSInteger response;

      [invocation setTarget:self];
      [invocation setSelector:_cmd];
      [invocation setArgument:&absoluteDirectoryPath atIndex:2];
      [invocation setArgument:&filename atIndex:3];
      [invocation setArgument:&fileTypes atIndex:4];

      [NSApp runTemporarilyWithInvocation:invocation];

      [invocation getReturnValue:&response];

      return response;
    }
}

@end				// EmacsOpenPanel

/* The actual implementation of Fx_file_dialog.  */

Lisp_Object
mac_file_dialog (prompt, dir, default_filename, mustmatch, only_dir_p)
     Lisp_Object prompt, dir, default_filename, mustmatch, only_dir_p;
{
  Lisp_Object file = Qnil;
  int count = SPECPDL_INDEX ();
  struct gcpro gcpro1, gcpro2, gcpro3, gcpro4, gcpro5, gcpro6;
  NSString *directory = nil, *nondirectory = nil;

  check_mac ();

  GCPRO6 (prompt, dir, default_filename, mustmatch, file, only_dir_p);
  CHECK_STRING (prompt);
  CHECK_STRING (dir);

  BLOCK_INPUT;

  if (STRINGP (default_filename))
    {
      Lisp_Object tem;

      tem = Ffile_name_directory (default_filename);
      directory = [NSString stringWithLispString:tem];
      tem = Ffile_name_nondirectory (default_filename);
      nondirectory = [NSString stringWithLispString:tem];
    }

  if (directory == nil)
    {
      dir = Fexpand_file_name (dir, Qnil);
      directory = [NSString stringWithLispString:dir];
    }

  if (NILP (only_dir_p) && NILP (mustmatch))
    {
      /* This is a save dialog */
      NSSavePanel *savePanel = [EmacsSavePanel savePanel];
      NSInteger response;

      [savePanel setTitle:[NSString stringWithLispString:prompt]];
      [savePanel setPrompt:@"OK"];
      if ([savePanel respondsToSelector:@selector(setNameFieldLabel:)])
	[savePanel setNameFieldLabel:@"Enter Name:"];

      if (directory)
	response = [savePanel runModalForDirectory:directory
			      file:nondirectory];
      else
	response = [savePanel runModal];

      if (response == NSFileHandlingPanelOKButton)
	file = [[savePanel filename] lispString];
    }
  else
    {
      /* This is an open dialog */
      NSOpenPanel *openPanel = [EmacsOpenPanel openPanel];
      NSInteger response;

      [openPanel setTitle:[NSString stringWithLispString:prompt]];
      [openPanel setPrompt:@"OK"];
      [openPanel setAllowsMultipleSelection:NO];
      [openPanel setCanChooseDirectories:YES];
      [openPanel setCanChooseFiles:(NILP (only_dir_p))];

      if (directory)
	response = [openPanel runModalForDirectory:directory
			      file:nondirectory types:nil];
      else
	response = [openPanel runModalForTypes:nil];

      if (response == NSOKButton)
	file = [[[openPanel filenames] objectAtIndex:0] lispString];
    }

  UNBLOCK_INPUT;

  UNGCPRO;

  /* Make "Cancel" equivalent to C-g.  */
  if (NILP (file))
    Fsignal (Qquit, Qnil);

  return unbind_to (count, file);
}


/***********************************************************************
			Font selection dialog
 ***********************************************************************/

@implementation EmacsFontDialogController

- (void)windowWillClose:(NSNotification *)aNotification
{
  [NSApp abortModal];
}

- (void)cancel:(id)sender
{
  [NSApp abortModal];
}

- (void)ok:(id)sender
{
  [NSApp stopModal];
}

- (void)changeFont:(id)sender
{
}

- (NSUInteger)validModesForFontPanel:(NSFontPanel *)fontPanel
{
  /* Underline, Strikethrough, TextColor, DocumentColor, and Shadow
     are not used in font panels.  */
  return (NSFontPanelFaceModeMask
	  | NSFontPanelSizeModeMask
	  | NSFontPanelCollectionModeMask);
}

@end				// EmacsFontDialogController

@implementation NSFontPanel (Emacs)

- (NSInteger)runModal
{
  NSMethodSignature *signature =
    [NSApp methodSignatureForSelector:@selector(runModalForWindow:)];
  NSInvocation *invocation =
    [NSInvocation invocationWithMethodSignature:signature];
  NSInteger response;

  [invocation setTarget:NSApp];
  [invocation setSelector:@selector(runModalForWindow:)];
  [invocation setArgument:&self atIndex:2];

  [NSApp runTemporarilyWithInvocation:invocation];

  [invocation getReturnValue:&response];

  return response;
}

@end				// NSFontPanel (Emacs)

static NSView *
create_ok_cancel_buttons_view ()
{
  NSMatrix *view;
  NSButtonCell *prototype = [[NSButtonCell alloc] init];
  NSSize cellSize;
  NSRect frame;
  NSButtonCell *cancelButton, *okButton;

  [prototype setBezelStyle:NSRoundedBezelStyle];
  cellSize = [prototype cellSize];
  frame = NSMakeRect (0, 0, cellSize.width * 2, cellSize.height);
  view = [[NSMatrix alloc] initWithFrame:frame
				    mode:NSTrackModeMatrix
				    prototype:prototype
				    numberOfRows:1 numberOfColumns:2];
  [prototype release];
  cancelButton = [view cellAtRow:0 column:0];
  okButton = [view cellAtRow:0 column:1];
  [cancelButton setTitle:@"Cancel"];
  [okButton setTitle:@"OK"];
  [cancelButton setAction:@selector(cancel:)];
  [okButton setAction:@selector(ok:)];
  [cancelButton setKeyEquivalent:@"\e"];
  [okButton setKeyEquivalent:@"\r"];

  return view;
}

Lisp_Object
mac_font_dialog (f)
     FRAME_PTR f;
{
  Lisp_Object result = Qnil;
  NSFontManager *fontManager = [NSFontManager sharedFontManager];
  NSFontPanel *fontPanel = [fontManager fontPanel:YES];
  NSFont *savedSelectedFont, *selectedFont;
  BOOL savedIsMultiple;
  NSView *savedAccessoryView, *accessoryView;
  id savedDelegate, delegate;
  NSInteger response;

  savedSelectedFont = [fontManager selectedFont];
  savedIsMultiple = [fontManager isMultiple];
  selectedFont = macfont_get_nsctfont (FRAME_FONT (f));
  [fontManager setSelectedFont:selectedFont isMultiple:NO];

  savedAccessoryView = [fontPanel accessoryView];
  accessoryView = create_ok_cancel_buttons_view ();
  [fontPanel setAccessoryView:accessoryView];
  [accessoryView release];

  savedDelegate = [fontPanel delegate];
  delegate = [[EmacsFontDialogController alloc] init];
  [fontPanel setDelegate:delegate];

  [fontManager orderFrontFontPanel:nil];
  response = [fontPanel runModal];
  if (response != NSRunAbortedResponse)
    {
      selectedFont = [fontManager convertFont:[fontManager selectedFont]];
      result = macfont_nsctfont_to_spec (selectedFont);
    }

  [fontPanel setAccessoryView:savedAccessoryView];
  [fontPanel setDelegate:savedDelegate];
  [delegate release];
  [fontManager setSelectedFont:savedSelectedFont isMultiple:savedIsMultiple];

  [fontPanel close];

  return result;
}


/************************************************************************
				 Menu
 ************************************************************************/

extern int popup_activated_flag;
extern int name_is_separator P_ ((const char *));
extern void find_and_call_menu_selection P_ ((FRAME_PTR, int, Lisp_Object,
					      void *));
extern void set_frame_menubar P_ ((FRAME_PTR, int, int));

static void update_services_menu_types P_ ((void));

@implementation NSMenu (Emacs)

/* Create a new menu item using the information in *WV (except
   submenus) and add it to the end of the receiver.  */

- (NSMenuItem *)addItemWithWidgetValue:(widget_value *)wv
{
  NSMenuItem *item;

  if (name_is_separator (wv->name))
    {
      item = (NSMenuItem *) [NSMenuItem separatorItem];
      [self addItem:item];
    }
  else
    {
      NSString *itemName = [NSString stringWithUTF8String:wv->name
				     fallback:YES];
      NSData *data;

      if (wv->key != NULL)
	itemName = [NSString stringWithFormat:@"%@\t%@", itemName,
			     [NSString stringWithUTF8String:wv->key
				       fallback:YES]];

      item = (NSMenuItem *) [self addItemWithTitle:itemName
				  action:@selector(setMenuItemSelectionToTag:)
				  keyEquivalent:@""];

      [item setEnabled:wv->enabled];

      /* We can't use [NSValue valueWithBytes:&wv->help
	 objCType:@encode(Lisp_Object)] when USE_LISP_UNION_TYPE
	 defined, because NSGetSizeAndAlignment does not support bit
	 fields (at least as of Mac OS X 10.5).  */
      data = [NSData dataWithBytes:&wv->help length:(sizeof (Lisp_Object))];
      [item setRepresentedObject:data];

      /* Draw radio buttons and tickboxes. */
      if (wv->selected && (wv->button_type == BUTTON_TYPE_TOGGLE
			   || wv->button_type == BUTTON_TYPE_RADIO))
	[item setState:NSOnState];
      else
	[item setState:NSOffState];

      [item setTag:((NSInteger) wv->call_data)];
    }

  return item;
}

/* Create menu trees defined by WV and add them to the end of the
   receiver.  */

- (void)fillWithWidgetValue:(widget_value *)first_wv
{
  widget_value *wv;
  NSFont *menuFont = [NSFont menuFontOfSize:0];
  NSDictionary *attributes =
    [NSDictionary dictionaryWithObject:menuFont forKey:NSFontAttributeName];
  NSSize spaceSize = [@" " sizeWithAttributes:attributes];
  CGFloat maxTabStop = 0;

  for (wv = first_wv; wv != NULL; wv = wv->next)
    if (!name_is_separator (wv->name) && wv->key)
      {
	NSString *itemName =
	  [NSString stringWithUTF8String:wv->name fallback:YES];
	NSSize size = [[itemName stringByAppendingString:@"\t"]
			sizeWithAttributes:attributes];

	if (maxTabStop < size.width)
	  maxTabStop = size.width;
      }

  for (wv = first_wv; wv != NULL; wv = wv->next)
    if (!name_is_separator (wv->name) && wv->key)
      {
	NSString *itemName =
	  [NSString stringWithUTF8String:wv->name fallback:YES];
	NSSize nameSize = [itemName sizeWithAttributes:attributes];
	int name_len = strlen (wv->name);
	int pad_len = ceil ((maxTabStop - nameSize.width) / spaceSize.width);
	Lisp_Object name;

	name = make_uninit_string (name_len + pad_len);
	strcpy (SDATA (name), wv->name);
	memset (SDATA (name) + name_len, ' ', pad_len);
	wv->name = SDATA (name);
      }

  for (wv = first_wv; wv != NULL; wv = wv->next)
    {
      NSMenuItem *item = [self addItemWithWidgetValue:wv];

      if (wv->contents)
	{
	  NSMenu *submenu = [[NSMenu alloc] initWithTitle:@"Submenu"];

	  [submenu setAutoenablesItems:NO];
	  [self setSubmenu:submenu forItem:item];
	  [submenu fillWithWidgetValue:wv->contents];
	  [submenu release];
	}
    }

  if (!(floor (NSAppKitVersionNumber) <= NSAppKitVersionNumber10_4))
    [self setDelegate:[NSApp delegate]];
}

@end				// NSMenu (Emacs)

@implementation EmacsMenu

/* Forward unprocessed shortcut key events to the first responder of
   the key window.  */

- (BOOL)performKeyEquivalent:(NSEvent *)theEvent
{
  NSWindow *window;
  NSResponder *firstResponder;

  if ([super performKeyEquivalent:theEvent])
    return YES;

  /* Special handling is required for Command+Space or
     Command+Option+Space on Mac OS X 10.3 or earlier.  This method
     should return NO in order to make input source changes work.  */
  if (floor (NSAppKitVersionNumber) <= NSAppKitVersionNumber10_3
      && [[theEvent charactersIgnoringModifiers] isEqualToString:@" "])
    {
      NSUInteger flags = [theEvent modifierFlags];

      flags &= ANY_KEY_MODIFIER_FLAGS_MASK;

      if (flags == NSCommandKeyMask)
	return NO;

      if (flags == (NSCommandKeyMask | NSAlternateKeyMask))
	{
	  NSUserDefaults *userDefaults = [NSUserDefaults standardUserDefaults];
	  NSDictionary *dict =
	    [userDefaults persistentDomainForName:@"com.apple.HIToolbox"];
	  id value = [dict valueForKey:@"AppleCommandOptionSpace"];

	  if (value == nil || [value boolValue])
	    return NO;
	}
    }

  window = [NSApp keyWindow];
  if (window == nil)
    window = FRAME_MAC_WINDOW (SELECTED_FRAME ());
  firstResponder = [window firstResponder];
  if ([firstResponder isMemberOfClass:[EmacsView class]])
    {
      [firstResponder keyDown:theEvent];

      return YES;
    }
  else if ([theEvent type] == NSKeyDown)
    {
      NSUInteger flags = [theEvent modifierFlags];

      flags &= ANY_KEY_MODIFIER_FLAGS_MASK;

      if (flags == NSCommandKeyMask)
	{
	  NSString *characters = [theEvent charactersIgnoringModifiers];
	  SEL action = NULL;

	  if ([characters isEqualToString:@"x"])
	    action = @selector(cut:);
	  else if ([characters isEqualToString:@"c"])
	    action = @selector(copy:);
	  else if ([characters isEqualToString:@"v"])
	    action = @selector(paste:);

	  if (action)
	    return [NSApp sendAction:action to:nil from:nil];
	}
    }

  return NO;
}

@end				// EmacsMenu

@implementation EmacsController (Menu)

extern Lisp_Object Vshow_help_function;

static Lisp_Object
restore_show_help_function (old_show_help_function)
     Lisp_Object old_show_help_function;
{
  Vshow_help_function = old_show_help_function;

  return Qnil;
}

- (void)menu:(NSMenu *)menu willHighlightItem:(NSMenuItem *)item
{
  id object = [item representedObject];
  Lisp_Object help;
  int specpdl_count = SPECPDL_INDEX ();

  if (object)
    [object getBytes:&help];
  else
    help = Qnil;

  /* Temporarily bind Vshow_help_function to Qnil because we don't
     want tooltips during menu tracking.  */
  record_unwind_protect (restore_show_help_function, Vshow_help_function);
  Vshow_help_function = intern ("tooltip-show-help-non-mode");

  show_help_echo (help, Qnil, Qnil, Qnil, 1);
  unbind_to (specpdl_count, Qnil);
}

/* Start menu bar tracking and return when it is completed.

   The tracking is done inside the application loop because otherwise
   we can't pop down an error dialog caused by a Service invocation,
   for example.  */

- (void)trackMenubar
{
  if ([NSApp isRunning])
    {
      /* Mac OS X 10.2 doesn't regard untilDate:nil as polling.  */
      NSDate *expiration = [NSDate distantPast];

      while (1)
	{
	  NSEvent *event = [NSApp nextEventMatchingMask:NSAnyEventMask
				  untilDate:expiration
				  inMode:NSDefaultRunLoopMode dequeue:YES];

	  if (event == nil)
	    {
	      /* There can be a pending mouse down event on the menu
		 bar at least on 10.5 with Command-Shift-/ -> search
		 with keyword -> select.  */
	      if (peek_if_next_event_activates_menu_bar () == NULL)
		break;
	    }
	  else if (NSEventMaskFromType ([event type]) & ANY_MOUSE_EVENT_MASK)
	    [NSApp sendEvent:event];
	  else
	    {
	      [NSApp postEvent:event atStart:YES];
	      break;
	    }
	}
    }
  else
    {
      NSMethodSignature *signature = [self methodSignatureForSelector:_cmd];
      NSInvocation *invocation =
	[NSInvocation invocationWithMethodSignature:signature];

      [invocation setTarget:self];
      [invocation setSelector:_cmd];

      [NSApp runTemporarilyWithInvocation:invocation];
    }
}

@end				// EmacsController (Menu)

/* Activate the menu bar of frame F.
   This is called from keyboard.c when it gets the
   MENU_BAR_ACTIVATE_EVENT out of the Emacs event queue.

   To activate the menu bar, we use the button-press event that was
   saved in dpyinfo->saved_menu_event.

   But first we recompute the menu bar contents (the whole tree).

   The reason for saving the button event until here, instead of
   passing it to the toolkit right away, is that we can safely
   execute Lisp code.  */

void
x_activate_menubar (f)
     FRAME_PTR f;
{
  struct mac_display_info *dpyinfo = FRAME_MAC_DISPLAY_INFO (f);
  EventRef menu_event = dpyinfo->saved_menu_event;

  if (menu_event == NULL)
    return;

  dpyinfo->saved_menu_event = NULL;

  set_frame_menubar (f, 0, 1);
  BLOCK_INPUT;
  update_services_menu_types ();
  menu_item_selection = 0;
  popup_activated_flag = 1;
  PostEventToQueue (GetMainEventQueue (), menu_event, kEventPriorityHigh);
  ReleaseEvent (menu_event);
  [[NSApp delegate] trackMenubar];
  popup_activated_flag = 0;
  UNBLOCK_INPUT;

  if (menu_item_selection)
    find_and_call_menu_selection (f, f->menu_bar_items_used, f->menu_bar_vector,
				  (void *) (intptr_t) menu_item_selection);
}

/* Set up the initial menu bar.  */

static void
init_menu_bar ()
{
  NSMenu *servicesMenu = [[NSMenu alloc] init];
  NSMenu *windowsMenu = [[NSMenu alloc] init];
  NSMenu *appleMenu = [[NSMenu alloc] init];
  EmacsMenu *mainMenu = [[EmacsMenu alloc] init];

  [NSApp setServicesMenu:servicesMenu];

  [NSApp setWindowsMenu:windowsMenu];

  [appleMenu addItemWithTitle:@"About Emacs"
	     action:@selector(about:)
	     keyEquivalent:@""];
  [appleMenu addItem:[NSMenuItem separatorItem]];
  [appleMenu addItemWithTitle:@"Preferences..."
	     action:@selector(preferences:) keyEquivalent:@""];
  [appleMenu addItem:[NSMenuItem separatorItem]];
  [appleMenu setSubmenu:servicesMenu
	     forItem:[appleMenu addItemWithTitle:@"Services"
				action:nil keyEquivalent:@""]];
  [appleMenu addItem:[NSMenuItem separatorItem]];
  [appleMenu addItemWithTitle:@"Hide Emacs"
	     action:@selector(hide:) keyEquivalent:@"h"];
  [[appleMenu addItemWithTitle:@"Hide Others"
	      action:@selector(hideOtherApplications:) keyEquivalent:@"h"]
    setKeyEquivalentModifierMask:(NSAlternateKeyMask | NSCommandKeyMask)];
  [appleMenu addItemWithTitle:@"Show All"
	     action:@selector(unhideAllApplications:) keyEquivalent:@""];
  [appleMenu addItem:[NSMenuItem separatorItem]];
  [appleMenu addItemWithTitle:@"Quit Emacs"
	     action:@selector(terminate:) keyEquivalent:@""];
  /* -[NSApplication setAppleMenu:] is hidden on Mac OS X 10.4.  */
  [NSApp performSelector:@selector(setAppleMenu:) withObject:appleMenu];

  [mainMenu setAutoenablesItems:NO];
  [mainMenu setSubmenu:appleMenu
	    forItem:[mainMenu addItemWithTitle:@""
			      action:nil keyEquivalent:@""]];
  [NSApp setMainMenu:mainMenu];

  [mainMenu release];
  [appleMenu release];
  [windowsMenu release];
  [servicesMenu release];
}

/* Fill menu bar with the items defined by WV.  If DEEP_P, consider
   the entire menu trees we supply, rather than just the menu bar item
   names.  */

void
mac_fill_menubar (wv, deep_p)
     widget_value *wv;
     int deep_p;
{
  NSMenu *mainMenu = [NSApp mainMenu];
  NSInteger index, nitems = [mainMenu numberOfItems];

  if (deep_p)
    while (nitems > 1)
      [mainMenu removeItemAtIndex:--nitems];

  for (index = 1; wv != NULL; wv = wv->next, index++)
    {
      NSString *title = ((NSString *)
			 CFStringCreateWithCString (NULL, wv->name,
						    kCFStringEncodingMacRoman));
      NSMenu *submenu;

      if (index < nitems)
	{
	  submenu = [[mainMenu itemAtIndex:index] submenu];

	  if (submenu && [submenu numberOfItems] == 0
	      && [title isEqualToString:[submenu title]])
	    {
	      [title release];
	      continue;
	    }
	  else
	    while (nitems > index)
	      [mainMenu removeItemAtIndex:--nitems];
	}

      submenu = [[NSMenu alloc] initWithTitle:title];
      [submenu setAutoenablesItems:NO];

      [mainMenu setSubmenu:submenu
		forItem:[mainMenu addItemWithTitle:title action:nil
				  keyEquivalent:@""]];
      [title release];

      if (wv->contents)
	[submenu fillWithWidgetValue:wv->contents];

      [submenu release];
    }

  while (nitems > index)
    [mainMenu removeItemAtIndex:--nitems];
}

static Lisp_Object
pop_down_menu (arg)
     Lisp_Object arg;
{
  struct Lisp_Save_Value *p = XSAVE_VALUE (arg);
  NSMenu *menu = p->pointer;

  BLOCK_INPUT;

  /* Must reset this manually because the button release event is not
     passed to Emacs event loop. */
  FRAME_MAC_DISPLAY_INFO (f)->grabbed = 0;

  [menu release];

  UNBLOCK_INPUT;

  return Qnil;
}

/* Pop up the menu for frame F defined by FIRST_WV at X/Y and loop until the
   menu pops down.
   menu_item_selection will be set to the selection.  */

void
create_and_show_popup_menu (f, first_wv, x, y, for_click)
     FRAME_PTR f;
     widget_value *first_wv;
     int x;
     int y;
     int for_click;
{
  NSMenu *menu = [[NSMenu alloc] initWithTitle:@"Popup"];
  NSWindow *window = FRAME_MAC_WINDOW (f);
  EmacsView *emacsView = FRAME_EMACS_VIEW (f);
  NSPoint location = [emacsView convertPoint:(NSMakePoint (x, y)) toView:nil];
  NSEvent *event;
  int specpdl_count = SPECPDL_INDEX ();

  [menu setAutoenablesItems:NO];
  [menu fillWithWidgetValue:first_wv->contents];

  record_unwind_protect (pop_down_menu, make_save_value (menu, 0));
  event = [NSEvent mouseEventWithType:NSLeftMouseDown location:location
		   modifierFlags:0 timestamp:0
		   windowNumber:[window windowNumber]
		   context:[NSGraphicsContext currentContext]
		   eventNumber:0 clickCount:1 pressure:0];
  popup_activated_flag = 1;
  [NSMenu popUpContextMenu:menu withEvent:event forView:emacsView];
  popup_activated_flag = 0;

  unbind_to (specpdl_count, Qnil);
}


/***********************************************************************
			     Popup Dialog
 ***********************************************************************/

extern EMACS_TIME timer_check P_ ((int));

@implementation EmacsDialogView

#define DIALOG_BUTTON_BORDER (6)
#define DIALOG_TEXT_BORDER (1)

- (BOOL)isFlipped
{
  return YES;
}

- (id)initWithWidgetValue:(widget_value *)wv
{
  char *dialog_name;
  int nb_buttons, first_group_count, i;
  CGFloat buttons_height, text_height, inner_width, inner_height;
  NSString *message;
  NSRect frameRect;
  NSButton **buttons, *defaultButton = nil;
  NSTextField *text;
  NSImageView *icon;

  self = [self init];

  if (self == nil)
    return nil;

  dialog_name = wv->name;
  nb_buttons = dialog_name[1] - '0';
  first_group_count = nb_buttons - (dialog_name[4] - '0');

  wv = wv->contents;
  message = [NSString stringWithUTF8String:wv->value fallback:YES];

  wv = wv->next;

  buttons = alloca (sizeof (NSButton *) * nb_buttons);

  for (i = 0; i < nb_buttons; i++)
    {
      NSButton *button = [[NSButton alloc] init];
      NSString *label = [NSString stringWithUTF8String:wv->value fallback:YES];

      [self addSubview:button];
      [button release];

      [button setBezelStyle:NSRoundedBezelStyle];
      [button setFont:[NSFont systemFontOfSize:0]];
      [button setTitle:label];

      [button setEnabled:wv->enabled];
      if (defaultButton == nil)
	defaultButton = button;

      [button sizeToFit];
      frameRect = [button frame];
      if (frameRect.size.width < (DIALOG_BUTTON_MIN_WIDTH
				  + DIALOG_BUTTON_BORDER * 2))
	frameRect.size.width = (DIALOG_BUTTON_MIN_WIDTH
				+ DIALOG_BUTTON_BORDER * 2);
      else if (frameRect.size.width > (DIALOG_MAX_INNER_WIDTH
				       + DIALOG_BUTTON_BORDER * 2))
	frameRect.size.width = (DIALOG_MAX_INNER_WIDTH
				+ DIALOG_BUTTON_BORDER * 2);
      [button setFrameSize:frameRect.size];

      [button setTag:((NSInteger) wv->call_data)];
      [button setTarget:self];
      [button setAction:@selector(stopModalWithTagAsCode:)];

      buttons[i] = button;
      wv = wv->next;
    }

  /* Layout buttons.  [buttons[i] frame] is set relative to the
     bottom-right corner of the inner box.  */
  {
    CGFloat bottom, right, max_height, left_align_shift;
    CGFloat button_cell_width, button_cell_height;
    NSButton *button;

    inner_width = DIALOG_MIN_INNER_WIDTH;
    bottom = right = max_height = 0;

    for (i = 0; i < nb_buttons; i++)
      {
	button = buttons[i];
	frameRect = [button frame];
	button_cell_width = NSWidth (frameRect) - DIALOG_BUTTON_BORDER * 2;
	button_cell_height = NSHeight (frameRect) - DIALOG_BUTTON_BORDER * 2;
	if (right - button_cell_width < - inner_width)
	  {
	    if (i != first_group_count
		&& right - button_cell_width >= - DIALOG_MAX_INNER_WIDTH)
	      inner_width = - (right - button_cell_width);
	    else
	      {
		bottom -= max_height + DIALOG_BUTTON_BUTTON_VERTICAL_SPACE;
		right = max_height = 0;
	      }
	  }
	if (max_height < button_cell_height)
	  max_height = button_cell_height;
	frameRect.origin = NSMakePoint ((right - button_cell_width
					 - DIALOG_BUTTON_BORDER),
					(bottom - button_cell_height
					 - DIALOG_BUTTON_BORDER));
	[button setFrameOrigin:frameRect.origin];
	right = (NSMinX (frameRect) + DIALOG_BUTTON_BORDER
		 - DIALOG_BUTTON_BUTTON_HORIZONTAL_SPACE);
	if (i == first_group_count - 1)
	  right -= DIALOG_BUTTON_BUTTON_HORIZONTAL_SPACE;
      }
    buttons_height = - (bottom - max_height);

    left_align_shift = - (inner_width + NSMinX (frameRect)
			  + DIALOG_BUTTON_BORDER);
    for (i = nb_buttons - 1; i >= first_group_count; i--)
      {
	button = buttons[i];
	frameRect = [button frame];

	if (bottom != NSMaxY (frameRect) - DIALOG_BUTTON_BORDER)
	  {
	    left_align_shift = - (inner_width + NSMinX (frameRect)
				  + DIALOG_BUTTON_BORDER);
	    bottom = NSMaxY (frameRect) - DIALOG_BUTTON_BORDER;
	  }
	frameRect.origin.x += left_align_shift;
	[button setFrameOrigin:frameRect.origin];
      }
  }

  /* Create a static text control and measure its bounds.  */
  frameRect = NSMakeRect (0, 0, inner_width + DIALOG_TEXT_BORDER * 2, 0);
  text = [[NSTextField alloc] initWithFrame:frameRect];

  [self addSubview:text];
  [text release];

  [text setFont:[NSFont systemFontOfSize:0]];
  [text setStringValue:message];
  [text setDrawsBackground:NO];
  [text setSelectable:NO];
  [text setBezeled:NO];

  [text sizeToFit];
  frameRect = [text frame];
  text_height = NSHeight (frameRect) - DIALOG_TEXT_BORDER * 2;
  if (text_height < DIALOG_TEXT_MIN_HEIGHT)
    text_height = DIALOG_TEXT_MIN_HEIGHT;

  /* Place buttons. */
  inner_height = (text_height + DIALOG_TEXT_BUTTONS_VERTICAL_SPACE
		  + buttons_height);
  for (i = 0; i < nb_buttons; i++)
    {
      NSButton *button = buttons[i];

      frameRect = [button frame];
      frameRect.origin.x += DIALOG_LEFT_MARGIN + inner_width;
      frameRect.origin.y += DIALOG_TOP_MARGIN + inner_height;
      [button setFrameOrigin:frameRect.origin];
    }

  /* Place text.  */
  frameRect = NSMakeRect (DIALOG_LEFT_MARGIN - DIALOG_TEXT_BORDER,
			  DIALOG_TOP_MARGIN - DIALOG_TEXT_BORDER,
			  inner_width + DIALOG_TEXT_BORDER * 2,
			  text_height + DIALOG_TEXT_BORDER * 2);
  [text setFrame:frameRect];

  /* Create the application icon at the upper-left corner.  */
  frameRect = NSMakeRect (DIALOG_ICON_LEFT_MARGIN, DIALOG_ICON_TOP_MARGIN,
			  DIALOG_ICON_WIDTH, DIALOG_ICON_HEIGHT);
  icon = [[NSImageView alloc] initWithFrame:frameRect];
  [self addSubview:icon];
  [icon release];
  [icon setImage:[NSImage imageNamed:@"NSApplicationIcon"]];

  [defaultButton setKeyEquivalent:@"\r"];

  frameRect =
    NSMakeRect (0, 0,
		DIALOG_LEFT_MARGIN + inner_width + DIALOG_RIGHT_MARGIN,
		DIALOG_TOP_MARGIN + inner_height + DIALOG_BOTTOM_MARGIN);
  [self setFrame:frameRect];

  return self;
}

- (void)stopModalWithTagAsCode:(id)sender
{
  [NSApp stopModalWithCode:[sender tag]];
}

/* Pop down if escape or quit key is pressed.  */

- (BOOL)performKeyEquivalent:(NSEvent *)theEvent
{
  BOOL quit = NO;

  if ([theEvent type] == NSKeyDown)
    {
      NSString *characters = [theEvent characters];

      if ([characters length] == 1)
	{
	  if ([characters characterAtIndex:0] == '\033')
	    quit = YES;
	  else
	    {
	      NSUInteger flags = [theEvent modifierFlags];
	      UInt32 modifiers = mac_modifier_flags_to_modifiers (flags);

	      if (mac_quit_char_key_p (modifiers, [theEvent keyCode]))
		quit = YES;
	    }
	}
    }

  if (quit)
    {
      [NSApp stopModal];

      return YES;
    }

  return [super performKeyEquivalent:theEvent];
}

@end				// EmacsDialogView

static Lisp_Object
pop_down_dialog (arg)
     Lisp_Object arg;
{
  struct Lisp_Save_Value *p = XSAVE_VALUE (XCAR (arg));
  NSPanel *panel = p->pointer;
  NSModalSession session;

  memcpy (&session, SDATA (XCDR (arg)), sizeof (NSModalSession));

  BLOCK_INPUT;

  [panel close];
  [NSApp endModalSession:session];
  [panel release];
  popup_activated_flag = 0;

  UNBLOCK_INPUT;

  return Qnil;
}

/* Pop up the dialog for frame F defined by FIRST_WV and loop until the
   dialog pops down.
   menu_item_selection will be set to the selection.  */

void
create_and_show_dialog (f, first_wv)
     FRAME_PTR f;
     widget_value *first_wv;
{
  int result = 0;
  EmacsDialogView *dialogView =
    [[EmacsDialogView alloc] initWithWidgetValue:first_wv];
  NSPanel *panel = [[NSPanel alloc] initWithContentRect:[dialogView frame]
				    styleMask:NSTitledWindowMask
				    backing:NSBackingStoreBuffered defer:YES];
  NSWindow *window = FRAME_MAC_WINDOW (f);
  NSRect panelFrame, windowFrame, visibleFrame;

  panelFrame = [panel frame];
  windowFrame = [window frame];
  panelFrame.origin.x = floor (windowFrame.origin.x
			       + (NSWidth (windowFrame)
				  - NSWidth (panelFrame)) * 0.5f);
  if (NSHeight (panelFrame) < NSHeight (windowFrame))
    panelFrame.origin.y = floor (windowFrame.origin.y
				 + (NSHeight (windowFrame)
				    - NSHeight (panelFrame)) * 0.8f);
  else
    panelFrame.origin.y = NSMaxY (windowFrame) - NSHeight (panelFrame);

  visibleFrame = [[window screen] visibleFrame];
  if (NSMaxX (panelFrame) > NSMaxX (visibleFrame))
    panelFrame.origin.x -= NSMaxX (panelFrame) - NSMaxX (visibleFrame);
  if (NSMinX (panelFrame) < NSMinX (visibleFrame))
    panelFrame.origin.x += NSMinX (visibleFrame) - NSMinX (panelFrame);
  if (NSMinY (panelFrame) < NSMinY (visibleFrame))
    panelFrame.origin.y += NSMinY (visibleFrame) - NSMinY (panelFrame);
  if (NSMaxY (panelFrame) > NSMaxY (visibleFrame))
    panelFrame.origin.y -= NSMaxY (panelFrame) - NSMaxY (visibleFrame);

  [panel setFrameOrigin:panelFrame.origin];
  [panel setContentView:dialogView];
  [dialogView release];
  [panel setTitle:(first_wv->name[0] == 'Q' ? @"Question" : @"Information")];
  [panel makeKeyAndOrderFront:nil];

  popup_activated_flag = 1;
  {
    NSModalSession session = [NSApp beginModalSessionForWindow:panel];
    Lisp_Object session_obj =
      make_unibyte_string ((char *) &session, sizeof (NSModalSession));
    int specpdl_count = SPECPDL_INDEX ();
    NSInteger response;

    record_unwind_protect (pop_down_dialog,
			   Fcons (make_save_value (panel, 0), session_obj));
    do
      {
	EMACS_TIME next_time = timer_check (1);
	long secs = EMACS_SECS (next_time);
	long usecs = EMACS_USECS (next_time);

	if (secs < 0 || (secs == 0 && usecs == 0))
	  {
	    /* Sometimes timer_check returns -1 (no timers) even if
	       there are timers.  So do a timeout anyway.  */
	    secs = 1;
	    usecs = 0;
	  }
        mac_run_loop_run_once (secs + usecs * 0.000001);

	/* This is necessary on 10.5 to make the dialog visible when
	   the user tries logout/shutdown.  */
	[panel makeKeyAndOrderFront:nil];
	response = [NSApp runModalSession:session];
	if (response >= 0)
	  result = response;
      }
    while (response == NSRunContinuesResponse);

    unbind_to (specpdl_count, Qnil);
  }

  menu_item_selection = result;
}


/***********************************************************************
			  Selection support
***********************************************************************/

extern Lisp_Object Qmac_pasteboard_name, Qmac_pasteboard_data_type;
extern Lisp_Object Qstring, Qarray;
extern Lisp_Object Vselection_converter_alist;

@implementation NSPasteboard (Emacs)

/* Writes LISPOBJECT of the specified DATATYPE to the pasteboard
   server.  */

- (BOOL)setLispObject:(Lisp_Object)lispObject forType:(NSString *)dataType
{
  BOOL result = NO;

  if (dataType == nil)
    return NO;

  if ([dataType isEqualToString:NSFilenamesPboardType])
    {
      id propertyList = (id) cfproperty_list_create_with_lisp_data (lispObject);

      result = [self setPropertyList:propertyList forType:dataType];
    }
  else if ([dataType isEqualToString:NSStringPboardType]
	   || [dataType isEqualToString:NSTabularTextPboardType])
    {
      NSString *string = [NSString stringWithUTF8String:(SDATA (lispObject))
				   fallback:YES];

      result = [self setString:string forType:dataType];
    }
  else if ([dataType isEqualToString:NSURLPboardType])
    {
      NSString *string = [NSString stringWithUTF8String:(SDATA (lispObject))
				   fallback:YES];
      NSURL *url = [NSURL URLWithString:string];

      if (url)
	{
	  [url writeToPasteboard:self];
	  result = YES;
	}
    }
  else
    {
      NSData *data = [NSData dataWithBytes:(SDATA (lispObject))
			     length:(SBYTES (lispObject))];

      result = [self setData:data forType:dataType];
    }

  return result;
}

/* Return the Lisp object for the specified DATATYPE.  */

- (Lisp_Object)lispObjectForType:(NSString *)dataType
{
  Lisp_Object result = Qnil;

  if (dataType == nil)
    return Qnil;

  if ([dataType isEqualToString:NSFilenamesPboardType])
    {
      id propertyList = [self propertyListForType:dataType];

      if (propertyList)
	result = cfobject_to_lisp ((CFTypeRef) propertyList,
				   CFOBJECT_TO_LISP_FLAGS_FOR_EVENT, -1);
    }
  else if ([dataType isEqualToString:NSStringPboardType]
	   || [dataType isEqualToString:NSTabularTextPboardType])
    {
      NSString *string = [self stringForType:dataType];

      if (string)
	result = [string UTF8LispString];
    }
  else if ([dataType isEqualToString:NSURLPboardType])
    {
      NSURL *url = [NSURL URLFromPasteboard:self];

      if (url)
	result = [[url absoluteString] UTF8LispString];
    }
  else
    {
      NSData *data = [self dataForType:dataType];

      if (data)
	result = [data lispString];
    }

  return result;
}

@end				// NSPasteboard (Emacs)

/* Get a reference to the selection corresponding to the symbol SYM.
   The reference is set to *SEL, and it becomes NULL if there's no
   corresponding selection.  Clear the selection if CLEAR_P is
   non-zero.  */

OSStatus
mac_get_selection_from_symbol (sym, clear_p, sel)
     Lisp_Object sym;
     int clear_p;
     Selection *sel;
{
  Lisp_Object str = Fget (sym, Qmac_pasteboard_name);

  if (!STRINGP (str))
    *sel = NULL;
  else
    {
      NSString *name = [NSString stringWithLispString:str];

      *sel = [NSPasteboard pasteboardWithName:name];
      if (clear_p)
	[(NSPasteboard *)*sel declareTypes:[NSArray array] owner:nil];
    }

  return noErr;
}

/* Get a pasteboard data type from the symbol SYM.  Return nil if no
   corresponding data type.  If SEL is non-zero, the return value is
   non-zero only when the SEL has the data type.  */

static NSString *
get_pasteboard_data_type_from_symbol (sym, sel)
     Lisp_Object sym;
     Selection sel;
{
  Lisp_Object str = Fget (sym, Qmac_pasteboard_data_type);
  NSString *dataType;

  if (STRINGP (str))
    dataType = [NSString stringWithLispString:str];
  else
    dataType = nil;

  if (dataType && sel)
    {
      NSArray *array = [NSArray arrayWithObject:dataType];

      dataType = [(NSPasteboard *)sel availableTypeFromArray:array];
    }

  return dataType;
}

/* Check if the symbol SYM has a corresponding selection target type.  */

int
mac_valid_selection_target_p (sym)
     Lisp_Object sym;
{
  return STRINGP (Fget (sym, Qmac_pasteboard_data_type));
}

/* Clear the selection whose reference is *SEL.  */

OSStatus
mac_clear_selection (sel)
     Selection *sel;
{
  [(NSPasteboard *)*sel declareTypes:[NSArray array] owner:nil];

  return noErr;
}

/* Get ownership information for SEL.  Emacs can detect a change of
   the ownership by comparing saved and current values of the
   ownership information.  */

Lisp_Object
mac_get_selection_ownership_info (sel)
     Selection sel;
{
  return long_to_cons ([(NSPasteboard *)sel changeCount]);
}

/* Return non-zero if VALUE is a valid selection value for TARGET.  */

int
mac_valid_selection_value_p (value, target)
     Lisp_Object value, target;
{
  NSString *dataType;

  dataType = get_pasteboard_data_type_from_symbol (target, nil);
  if (dataType == nil)
    return 0;

  if ([dataType isEqualToString:NSFilenamesPboardType])
    {
      if (CONSP (value) && EQ (XCAR (value), Qarray)
	  && VECTORP (XCDR (value)))
	{
	  Lisp_Object vector = XCDR (value);
	  EMACS_INT i, size = ASIZE (vector);

	  for (i = 0; i < size; i++)
	    {
	      Lisp_Object elem = AREF (vector, i);

	      if (!(CONSP (elem) && EQ (XCAR (elem), Qstring)
		    && STRINGP (XCDR (elem))))
		break;
	    }

	  return i == size;
	}
    }
  else
    return STRINGP (value);

  return 0;
}

/* Put Lisp object VALUE to the selection SEL.  The target type is
   specified by TARGET. */

OSStatus
mac_put_selection_value (sel, target, value)
     Selection sel;
     Lisp_Object target, value;
{
  NSString *dataType = get_pasteboard_data_type_from_symbol (target, nil);
  NSPasteboard *pboard = (NSPasteboard *)sel;

  if (dataType == nil)
    return noTypeErr;

  [pboard addTypes:[NSArray arrayWithObject:dataType] owner:nil];

  return [pboard setLispObject:value forType:dataType] ? noErr : noTypeErr;
}

/* Check if data for the target type TARGET is available in SEL.  */

int
mac_selection_has_target_p (sel, target)
     Selection sel;
     Lisp_Object target;
{
  return get_pasteboard_data_type_from_symbol (target, sel) != nil;
}

/* Get data for the target type TARGET from SEL and create a Lisp
   object.  Return nil if failed to get data.  */

Lisp_Object
mac_get_selection_value (sel, target)
     Selection sel;
     Lisp_Object target;
{
  NSString *dataType = get_pasteboard_data_type_from_symbol (target, sel);

  if (dataType == nil)
    return Qnil;

  return [(NSPasteboard *)sel lispObjectForType:dataType];
}

/* Get the list of target types in SEL.  The return value is a list of
   target type symbols possibly followed by pasteboard data type
   strings.  */

Lisp_Object
mac_get_selection_target_list (sel)
     Selection sel;
{
  Lisp_Object result = Qnil, rest, target, strings = Qnil;
  NSArray *types = [(NSPasteboard *)sel types];
  NSMutableSet *typeSet;
  NSString *dataType;
  NSEnumerator *enumerator;

  typeSet = [NSMutableSet setWithCapacity:[types count]];
  [typeSet addObjectsFromArray:types];

  for (rest = Vselection_converter_alist; CONSP (rest); rest = XCDR (rest))
    if (CONSP (XCAR (rest))
	&& (target = XCAR (XCAR (rest)),
	    SYMBOLP (target))
	&& (dataType = get_pasteboard_data_type_from_symbol (target, sel)))
      {
	result = Fcons (target, result);
	[typeSet removeObject:dataType];
      }

  enumerator = [typeSet objectEnumerator];
  while ((dataType = [enumerator nextObject]) != nil)
    strings = Fcons ([dataType UTF8LispString], strings);
  result = nconc2 (result, strings);

  return result;
}


/***********************************************************************
			 Apple event support
***********************************************************************/

extern Lisp_Object Vmac_apple_event_map;
extern Lisp_Object Qmac_apple_event_class, Qmac_apple_event_id;
extern Lisp_Object Qundefined;

extern pascal OSErr mac_handle_apple_event P_ ((const AppleEvent *,
						AppleEvent *, SInt32));
extern void cleanup_all_suspended_apple_events P_ ((void));

static NSMutableSet *registered_apple_event_specs;

@implementation NSAppleEventDescriptor (Emacs)

- (OSErr)copyDescTo:(AEDesc *)desc
{
  return AEDuplicateDesc ([self aeDesc], desc);
}

@end				// NSAppleEventDescriptor (Emacs)

@implementation EmacsController (AppleEvent)

- (void)handleAppleEvent:(NSAppleEventDescriptor *)event
	  withReplyEvent:(NSAppleEventDescriptor *)replyEvent;
{
  OSErr err;
  AEDesc reply;

  err = [replyEvent copyDescTo:&reply];
  if (err == noErr)
    {
      const AEDesc *event_ptr = NULL;

      event_ptr = [event aeDesc];

      if (event_ptr)
	err = mac_handle_apple_event (event_ptr, &reply, 0);
      AEDisposeDesc (&reply);
    }
}

@end				// EmacsController (AppleEvent)

/* Function used as an argument to map_keymap for registering all
   pairs of Apple event class and ID in mac_apple_event_map.  */

static void
register_apple_event_specs (key, binding, args, data)
     Lisp_Object key, binding, args;
     void *data;
{
  Lisp_Object code_string;

  if (!SYMBOLP (key))
    return;
  code_string = Fget (key, (NILP (args)
			    ? Qmac_apple_event_class : Qmac_apple_event_id));
  if (STRINGP (code_string) && SBYTES (code_string) == 4)
    {
      if (NILP (args))
	{
	  Lisp_Object keymap = get_keymap (binding, 0, 0);

	  if (!NILP (keymap))
	    map_keymap (keymap, register_apple_event_specs,
			code_string, data, 0);
	}
      else if (!NILP (binding) && !EQ (binding, Qundefined))
	{
	  NSMutableSet *set = (NSMutableSet *) data;
	  AEEventClass eventClass;
	  AEEventID eventID;
	  unsigned long long code;
	  NSNumber *value;

	  eventID = EndianU32_BtoN (*((UInt32 *) SDATA (code_string)));
	  eventClass = EndianU32_BtoN (*((UInt32 *) SDATA (args)));
	  code = ((unsigned long long) eventClass << 32) + eventID;
	  value = [NSNumber numberWithUnsignedLongLong:code];

	  if (![set containsObject:value])
	    {
	      NSAppleEventManager *manager =
		[NSAppleEventManager sharedAppleEventManager];

	      [manager setEventHandler:[NSApp delegate]
		       andSelector:@selector(handleAppleEvent:withReplyEvent:)
		       forEventClass:eventClass andEventID:eventID];
	      [set addObject:value];
	    }
	}
    }
}

/* Register pairs of Apple event class and ID in mac_apple_event_map
   if they have not registered yet.  Each registered pair is stored in
   registered_apple_event_specs as a unsigned long long value whose
   upper and lower half stand for class and ID, respectively.  */

static void
update_apple_event_handler ()
{
  Lisp_Object keymap = get_keymap (Vmac_apple_event_map, 0, 0);

  if (!NILP (keymap))
    map_keymap (keymap, register_apple_event_specs, Qnil,
		registered_apple_event_specs, 0);
}

static void
init_apple_event_handler ()
{
  registered_apple_event_specs = [[NSMutableSet alloc] initWithCapacity:0];
  update_apple_event_handler ();
  atexit (cleanup_all_suspended_apple_events);
}


/***********************************************************************
                      Drag and drop support
***********************************************************************/

extern Lisp_Object Vmac_dnd_known_types;
extern Lisp_Object QCdata, QCtype;
extern Lisp_Object QCactions, Qcopy, Qlink, Qgeneric, Qprivate, Qmove, Qdelete;

static NSMutableArray *registered_dragged_types;

@implementation EmacsView (DragAndDrop)

- (NSDragOperation)draggingEntered:(id <NSDraggingInfo>)sender
{
  return NSDragOperationGeneric;
}

/* Convert the NSDragOperation value OPERATION to a list of symbols for
   the corresponding drag actions.  */

static Lisp_Object
drag_operation_to_actions (operation)
     NSDragOperation operation;
{
  Lisp_Object result = Qnil;

  if (operation & NSDragOperationCopy)
    result = Fcons (Qcopy, result);
  if (operation & NSDragOperationLink)
    result = Fcons (Qlink, result);
  if (operation & NSDragOperationGeneric)
    result = Fcons (Qgeneric, result);
  if (operation & NSDragOperationPrivate)
    result = Fcons (Qprivate, result);
  if (operation & NSDragOperationMove)
    result = Fcons (Qmove, result);
  if (operation & NSDragOperationDelete)
    result = Fcons (Qdelete, result);

  return result;
}

- (BOOL)performDragOperation:(id <NSDraggingInfo>)sender
{
  struct frame *f = [self emacsFrame];
  NSPoint point = [self convertPoint:[sender draggingLocation] fromView:nil];
  NSPasteboard *pboard = [sender draggingPasteboard];
  /* -[NSView registeredDraggedTypes] is available only on 10.4 and later.  */
  NSString *type = [pboard availableTypeFromArray:registered_dragged_types];
  NSDragOperation operation = [sender draggingSourceOperationMask];
  Lisp_Object arg;

  if (type == nil)
    return NO;

  arg = list2 (QCdata, [pboard lispObjectForType:type]);
  arg = Fcons (QCactions, Fcons (drag_operation_to_actions (operation), arg));
  arg = Fcons (QCtype, Fcons ([type UTF8LispString], arg));

  EVENT_INIT (inputEvent);
  inputEvent.kind = DRAG_N_DROP_EVENT;
  inputEvent.modifiers = 0;
  inputEvent.timestamp = [[NSApp currentEvent] timestamp] * 1000;
  XSETINT (inputEvent.x, point.x);
  XSETINT (inputEvent.y, point.y);
  XSETFRAME (inputEvent.frame_or_window, f);
  inputEvent.arg = arg;
  [self sendAction:action to:target];

  return YES;
}

@end				// EmacsView (DragAndDrop)

/* Update the pasteboard types derived from the value of
   mac-dnd-known-types and register them so every Emacs view can
   accept them.  The registered types are stored in
   registered_dragged_types.  */

static void
update_dragged_types ()
{
  NSMutableArray *array = [[NSMutableArray alloc] initWithCapacity:0];
  Lisp_Object rest, tail, frame;

  for (rest = Vmac_dnd_known_types; CONSP (rest); rest = XCDR (rest))
    if (STRINGP (XCAR (rest)))
      {
	/* We really want string_to_unibyte, but since it doesn't
	   exist yet, we use string_as_unibyte which works as well,
	   except for the fact that it's too permissive (it doesn't
	   check that the multibyte string only contain single-byte
	   chars).  */
	Lisp_Object type = Fstring_as_unibyte (XCAR (rest));
	NSString *typeString = [NSString stringWithLispString:type];

	if (typeString)
	  [array addObject:typeString];
      }

  FOR_EACH_FRAME (tail, frame)
    {
      struct frame *f = XFRAME (frame);

      if (EQ (frame, tip_frame))
	continue;

      if (FRAME_MAC_P (f))
	{
	  EmacsView *emacsView = FRAME_EMACS_VIEW (f);

	  [emacsView registerForDraggedTypes:array];
	}
    }

  [registered_dragged_types autorelease];
  registered_dragged_types = array;
}

/* Return default value for mac-dnd-known-types.  */

Lisp_Object
mac_dnd_default_known_types ()
{
  return list3 ([NSFilenamesPboardType UTF8LispString],
		[NSStringPboardType UTF8LispString],
		[NSTIFFPboardType UTF8LispString]);
}


/***********************************************************************
			Services menu support
***********************************************************************/

extern Lisp_Object Vmac_service_selection;
extern Lisp_Object Qservice, Qpaste, Qperform;

@implementation EmacsView (Services)

- (id)validRequestorForSendType:(NSString *)sendType
		     returnType:(NSString *)returnType
{
  NSPasteboard *pboard;
  NSArray *array;

  if ([sendType length] == 0
      || (!NILP (Fx_selection_owner_p (Vmac_service_selection))
	  && (mac_get_selection_from_symbol (Vmac_service_selection, 0,
					     (Selection *) &pboard) == noErr)
	  && pboard
	  && (array = [NSArray arrayWithObject:sendType],
	      [pboard availableTypeFromArray:array])))
    {
      Lisp_Object rest;
      NSString *dataType;

      if ([returnType length] == 0)
	return self;

      for (rest = Vselection_converter_alist; CONSP (rest);
	   rest = XCDR (rest))
	if (CONSP (XCAR (rest)) && SYMBOLP (XCAR (XCAR (rest)))
	    && (dataType =
		get_pasteboard_data_type_from_symbol (XCAR (XCAR (rest)), nil))
	    && [dataType isEqualToString:returnType])
	  return self;
    }

  return [super validRequestorForSendType:sendType returnType:returnType];
}

- (BOOL)writeSelectionToPasteboard:(NSPasteboard *)pboard
			     types:(NSArray *)types
{
  OSStatus err;
  NSPasteboard *servicePboard;
  NSArray *serviceTypes;
  NSEnumerator *enumerator;
  NSString *type;
  BOOL result = NO;

  err = mac_get_selection_from_symbol (Vmac_service_selection, 0,
				       (Selection *) &servicePboard);
  if (err != noErr || pboard == nil)
    return NO;

  serviceTypes = [servicePboard types];
  [pboard declareTypes:[NSArray array] owner:nil];

  enumerator = [types objectEnumerator];
  while ((type = [enumerator nextObject]) != nil)
    if ([serviceTypes containsObject:type])
      {
	NSData *data = [servicePboard dataForType:type];

	if (data)
	  {
	    [pboard addTypes:[NSArray arrayWithObject:type] owner:nil];
	    result = [pboard setData:data forType:type] || result;
	  }
      }

  return result;
}

/* Copy whole data of pasteboard PBOARD to the pasteboard specified by
   mac-service-selection.  */

static BOOL
copy_pasteboard_to_service_selection (pboard)
     NSPasteboard *pboard;
{
  OSStatus err;
  NSPasteboard *servicePboard;
  NSEnumerator *enumerator;
  NSString *type;
  BOOL result = NO;

  err = mac_get_selection_from_symbol (Vmac_service_selection, 1,
				       (Selection *) &servicePboard);
  if (err != noErr || servicePboard == nil)
    return NO;

  enumerator = [[pboard types] objectEnumerator];
  while ((type = [enumerator nextObject]) != nil)
    {
      NSData *data = [pboard dataForType:type];

      if (data)
	{
	  [servicePboard addTypes:[NSArray arrayWithObject:type] owner:nil];
	  result = [servicePboard setData:data forType:type] || result;
	}
    }

  return result;
}

- (BOOL)readSelectionFromPasteboard:(NSPasteboard *)pboard
{
  BOOL result = copy_pasteboard_to_service_selection (pboard);

  if (result)
    {
      OSStatus err;
      EventRef event;

      err = CreateEvent (NULL, kEventClassService, kEventServicePaste, 0,
			 kEventAttributeNone, &event);
      if (err == noErr)
	{
	  err = mac_store_event_ref_as_apple_event (0, 0, Qservice, Qpaste,
						    event, 0, NULL, NULL);
	  ReleaseEvent (event);
	}

      if (err != noErr)
	result = NO;
    }

  return result;
}

@end				// EmacsView (Services)

@implementation NSMethodSignature (Emacs)

/* Dummy method.  Just for getting its method signature.  */

- (void)messageName:(NSPasteboard *)pboard
	   userData:(NSString *)userData
	      error:(NSString **)error
{
}

@end				// NSMethodSignature (Emacs)

static BOOL
is_services_handler_selector (selector)
     SEL selector;
{
  NSString *name = NSStringFromSelector (selector);

  /* The selector name is of the form `MESSAGENAME:userData:error:' ?  */
  if ([name hasSuffix:@":userData:error:"]
      && (NSMaxRange ([name rangeOfString:@":"])
	  == [name length] - (sizeof ("userData:error:") - 1)))
    {
      /* Lookup the binding `[service perform MESSAGENAME]' in
	 mac-apple-event-map.  */
      Lisp_Object tem = get_keymap (Vmac_apple_event_map, 0, 0);

      if (!NILP (tem))
	tem = get_keymap (access_keymap (tem, Qservice, 0, 1, 0), 0, 0);
      if (!NILP (tem))
	tem = get_keymap (access_keymap (tem, Qperform, 0, 1, 0), 0, 0);
      if (!NILP (tem))
	{
	  NSUInteger index = [name length] - (sizeof (":userData:error:") - 1);

	  name = [name substringToIndex:index];
	  tem = access_keymap (tem, intern (SDATA ([name UTF8LispString])),
			       0, 1, 0);
	}
      if (!NILP (tem) && !EQ (tem, Qundefined))
	return YES;
    }

  return NO;
}

/* Return the method signature of services handlers.  */

static
NSMethodSignature *services_handler_signature ()
{
  static NSMethodSignature *signature;

  if (signature == nil)
    signature =
      [[NSMethodSignature instanceMethodSignatureForSelector:
			    @selector(messageName:userData:error:)]
	retain];

  return signature;
}

static void
handle_services_invocation (invocation)
     NSInvocation *invocation;
{
  NSPasteboard *pboard;
  NSString *userData;
  NSString **error;
  BOOL result;

  [invocation getArgument:&pboard atIndex:2];
  [invocation getArgument:&userData atIndex:3];
  [invocation getArgument:&error atIndex:4];

  result = copy_pasteboard_to_service_selection (pboard);
  if (result)
    {
      OSStatus err;
      EventRef event;

      err = CreateEvent (NULL, kEventClassService, kEventServicePerform,
			 0, kEventAttributeNone, &event);
      if (err == noErr)
	{
	  static const EventParamName names[] =
	    {kEventParamServiceMessageName, kEventParamServiceUserData};
	  static const EventParamType types[] =
	    {typeCFStringRef, typeCFStringRef};
	  NSString *name = NSStringFromSelector ([invocation selector]);
	  NSUInteger index;

	  index = [name length] - (sizeof (":userData:error:") - 1);
	  name = [name substringToIndex:index];

	  err = SetEventParameter (event, kEventParamServiceMessageName,
				   typeCFStringRef, sizeof (CFStringRef),
				   &name);
	  if (err == noErr)
	    err = SetEventParameter (event, kEventParamServiceUserData,
				     typeCFStringRef, sizeof (CFStringRef),
				     &userData);
	  if (err == noErr)
	    err = mac_store_event_ref_as_apple_event (0, 0, Qservice,
						      Qperform, event,
						      (sizeof (names)
						       / sizeof (names[0])),
						      names, types);
	  ReleaseEvent (event);
	}
    }
}

static void
update_services_menu_types ()
{
  NSMutableArray *array = [NSMutableArray arrayWithCapacity:0];
  Lisp_Object rest;

  for (rest = Vselection_converter_alist; CONSP (rest);
       rest = XCDR (rest))
    if (CONSP (XCAR (rest)) && SYMBOLP (XCAR (XCAR (rest))))
      {
	NSString *dataType =
	  get_pasteboard_data_type_from_symbol (XCAR (XCAR (rest)), nil);

	if (dataType)
	  [array addObject:dataType];
      }

  [NSApp registerServicesMenuSendTypes:array returnTypes:array];
}


/***********************************************************************
			    Action support
***********************************************************************/
extern Lisp_Object Qaction, Qmac_action_key_paths;

static BOOL
is_action_selector (selector)
     SEL selector;
{
  NSString *name = NSStringFromSelector (selector);

  /* The selector name is of the form `ACTIONNAME:' ?  */
  if (NSMaxRange ([name rangeOfString:@":"]) == [name length])
    {
      /* Lookup the binding `[action ACTIONNAME]' in
	 mac-apple-event-map.  */
      Lisp_Object tem = get_keymap (Vmac_apple_event_map, 0, 0);

      if (!NILP (tem))
	tem = get_keymap (access_keymap (tem, Qaction, 0, 1, 0), 0, 0);
      if (!NILP (tem))
	{
	  name = [name substringToIndex:([name length] - 1)];
	  tem = access_keymap (tem, intern (SDATA ([name UTF8LispString])),
			       0, 1, 0);
	}
      if (!NILP (tem) && !EQ (tem, Qundefined))
	return YES;
    }

  return NO;
}

/* Return the method signature of actions.  */

static
NSMethodSignature *action_signature ()
{
  static NSMethodSignature *signature;

  if (signature == nil)
    signature =
      [[NSApplication instanceMethodSignatureForSelector:@selector(terminate:)]
	retain];

  return signature;
}

static void
handle_action_invocation (invocation)
     NSInvocation *invocation;
{
  id sender;
  Lisp_Object arg = Qnil;
  struct input_event inev;
  NSString *name = NSStringFromSelector ([invocation selector]);
  Lisp_Object name_symbol =
    intern (SDATA ([[name substringToIndex:([name length] - 1)]
		     UTF8LispString]));
  NSUInteger flags = [[NSApp currentEvent] modifierFlags];
  UInt32 modifiers = mac_modifier_flags_to_modifiers (flags);

  modifiers = EndianU32_NtoB (modifiers);
  arg = Fcons (Fcons (build_string ("kmod"), /* kEventParamKeyModifiers */
		      Fcons (build_string ("magn"), /* typeUInt32 */
			     make_unibyte_string ((char *) &modifiers, 4))),
	       arg);

  [invocation getArgument:&sender atIndex:2];

  if (sender)
    {
      Lisp_Object rest;

      for (rest = Fget (name_symbol, Qmac_action_key_paths);
	   CONSP (rest); rest = XCDR (rest))
	if (STRINGP (XCAR (rest)))
	  {
	    NSString *keyPath;
	    id value;
	    Lisp_Object obj;

	    keyPath = [NSString stringWithUTF8String:(SDATA (XCAR (rest)))
				fallback:YES];

	    NS_DURING
	      value = [sender valueForKeyPath:keyPath];
	    NS_HANDLER
	      value = nil;
	    NS_ENDHANDLER

	    if (value == nil)
	      continue;
	    obj = cfobject_to_lisp ((CFTypeRef) value,
				    CFOBJECT_TO_LISP_FLAGS_FOR_EVENT, -1);
	    arg = Fcons (Fcons (XCAR (rest),
				Fcons (build_string ("Lisp"), obj)),
			 arg);
	  }

      if ([sender isKindOfClass:[NSView class]])
	{
	  id delegate = [[sender window] delegate];

	  if ([delegate isKindOfClass:[EmacsFrameController class]])
	    {
	      Lisp_Object frame;

	      XSETFRAME (frame, [delegate emacsFrame]);
	      arg = Fcons (Fcons (intern ("frame"),
				  Fcons (build_string ("Lisp"), frame)),
			 arg);
	    }
	}
    }

  EVENT_INIT (inev);
  inev.kind = MAC_APPLE_EVENT;
  inev.x = Qaction;
  inev.y = name_symbol;
  XSETFRAME (inev.frame_or_window,
	     mac_focus_frame (&one_mac_display_info));
  inev.arg = Fcons (build_string ("aevt"), arg);
  [[NSApp delegate] storeEvent:&inev];
}


/***********************************************************************
			 AppleScript support
***********************************************************************/

extern long do_applescript P_ ((Lisp_Object, Lisp_Object *));

@implementation EmacsController (AppleScript)

- (long)doAppleScript:(Lisp_Object)script result:(Lisp_Object *)result
{
  if ([NSApp isRunning])
    return do_applescript (script, result);
  else
    {
      NSMethodSignature *signature = [self methodSignatureForSelector:_cmd];
      NSInvocation *invocation =
	[NSInvocation invocationWithMethodSignature:signature];
      long osaerror;

      [invocation setTarget:self];
      [invocation setSelector:_cmd];
      [invocation setArgument:&script atIndex:2];
      [invocation setArgument:&result atIndex:3];

      [NSApp runTemporarilyWithInvocation:invocation];

      [invocation getReturnValue:&osaerror];

      return osaerror;
    }
}

@end				// EmacsController (OSA)

long
mac_appkit_do_applescript (script, result)
     Lisp_Object script, *result;
{
  return [[NSApp delegate] doAppleScript:script result:result];
}


/***********************************************************************
				Fonts
***********************************************************************/

#if MAC_OS_X_VERSION_MIN_REQUIRED < 1050

#define FONT_NAME_ATTRIBUTE (@"NSFontNameAttribute")
#define FONT_FAMILY_ATTRIBUTE (@"NSFontFamilyAttribute")
#define FONT_TRAITS_ATTRIBUTE (@"NSCTFontTraitsAttribute")
#define FONT_SIZE_ATTRIBUTE (@"NSFontSizeAttribute")
#define FONT_CASCADE_LIST_ATTRIBUTE (@"NSCTFontCascadeListAttribute")
#define FONT_CHARACTER_SET_ATTRIBUTE (@"NSCTFontCharacterSetAttribute")
#define FONT_LANGUAGES_ATTRIBUTE (@"NSCTFontLanguagesAttribute")
#define FONT_SYMBOLIC_TRAIT (@"NSCTFontSymbolicTrait")
#define FONT_WEIGHT_TRAIT (@"NSCTFontWeightTrait")
#define FONT_WIDTH_TRAIT (@"NSCTFontProportionTrait")
#define FONT_SLANT_TRAIT (@"NSCTFontSlantTrait")

const CFStringRef MAC_FONT_NAME_ATTRIBUTE = (CFStringRef) FONT_NAME_ATTRIBUTE;
const CFStringRef MAC_FONT_FAMILY_NAME_ATTRIBUTE = (CFStringRef) FONT_FAMILY_ATTRIBUTE;
const CFStringRef MAC_FONT_TRAITS_ATTRIBUTE = (CFStringRef) FONT_TRAITS_ATTRIBUTE;
const CFStringRef MAC_FONT_SIZE_ATTRIBUTE = (CFStringRef) FONT_SIZE_ATTRIBUTE;
const CFStringRef MAC_FONT_CASCADE_LIST_ATTRIBUTE = (CFStringRef) FONT_CASCADE_LIST_ATTRIBUTE;
const CFStringRef MAC_FONT_CHARACTER_SET_ATTRIBUTE = (CFStringRef) FONT_CHARACTER_SET_ATTRIBUTE;
const CFStringRef MAC_FONT_LANGUAGES_ATTRIBUTE = (CFStringRef) FONT_LANGUAGES_ATTRIBUTE;
const CFStringRef MAC_FONT_SYMBOLIC_TRAIT = (CFStringRef) FONT_SYMBOLIC_TRAIT;
const CFStringRef MAC_FONT_WEIGHT_TRAIT = (CFStringRef) FONT_WEIGHT_TRAIT;
const CFStringRef MAC_FONT_WIDTH_TRAIT = (CFStringRef) FONT_WIDTH_TRAIT;
const CFStringRef MAC_FONT_SLANT_TRAIT = (CFStringRef) FONT_SLANT_TRAIT;

static BOOL mac_font_name_is_bogus P_ ((NSString *));
static NSNumber *mac_font_weight_override_for_name P_ ((NSString *));

static BOOL
mac_font_name_is_bogus (fontName)
     NSString *fontName;
{
  return ([fontName hasPrefix:@"."]
	  || ([fontName hasSuffix:@"Oblique"]
	      && ([fontName isEqualToString:@"Courier-Oblique"]
		  || [fontName isEqualToString:@"Courier-BoldOblique"]
		  || [fontName isEqualToString:@"Helvetica-Oblique"]
		  || [fontName isEqualToString:@"Helvetica-BoldOblique"])));
}

/* We override some weight trait values returned by NSFontDescriptor
   in 10.4, so that they match with those returned by Core Text.  */

static const struct
{
  NSString *fontName;
  const float weight;
} mac_font_weight_overrides [] =
  {{@"HiraKakuPro-W3", 0},	/* -0.23 in 10.4 */
   {@"HiraMinPro-W3", 0},	/* -0.23 in 10.4 */
   {@"HiraKakuPro-W6", 0.4},	/* 0.3 in 10.4 */
   {@"HiraMinPro-W6", 0.4},	/* 0.3 in 10.4 */
   {@"STFangsong", -0.4},	/* (5 - 5) * 0.1 in 10.3 */
   {@"STHeiti", 0.24},		/* (5 - 5) * 0.1 in 10.3 */
   {@"STXihei", -0.1},		/* (3 - 5) * 0.1 in 10.3 */
   {@"LiHeiPro", 0}};		/* 0.23 in 10.4 */

static NSNumber *
mac_font_weight_override_for_name (fontName)
     NSString *fontName;
{
  int i;

  for (i = 0; i < (sizeof (mac_font_weight_overrides)
		   / sizeof (mac_font_weight_overrides[0])); i++)
    if ([fontName isEqualToString:mac_font_weight_overrides[i].fontName])
      return [NSNumber numberWithFloat:mac_font_weight_overrides[i].weight];

  return nil;
}

static Boolean get_glyphs_for_characters P_ ((NSFont *, const UniChar [],
					      CGGlyph [], CFIndex));

/* Like CTFontGetGlyphsForCharacters, but without cache.  This must be
   used only in the cache implementation.  */

static Boolean
get_glyphs_for_characters (font, characters, glyphs, count)
     NSFont *font;
     const UniChar characters[];
     CGGlyph glyphs[];
     CFIndex count;
{
  Boolean result = true;
  NSString *string = [NSString stringWithCharacters:characters length:count];
  NSDictionary *attributes = [NSDictionary dictionaryWithObject:font
					   forKey:NSFontAttributeName];
  NSAttributedString *attributedString
    = [[NSAttributedString alloc] initWithString:string attributes:attributes];
  NSTextStorage *textStorage;
  NSLayoutManager *layoutManager;
  NSTextContainer *textContainer;
  NSString *fontName = [font fontName];
  CFIndex i;

  textStorage = [[NSTextStorage alloc] init];
  layoutManager = [[NSLayoutManager alloc] init];
  textContainer = [[NSTextContainer alloc] init];

  [layoutManager addTextContainer:textContainer];
  [textContainer release];
  [textStorage addLayoutManager:layoutManager];
  [layoutManager release];

  i = 0;
  while (i < count)
    {
      NSRange range = NSMakeRange (i, (characters[i] >= 0xD800
				       && characters[i] < 0xDC00) ? 2 : 1);
      NSAttributedString *attributedSubstring
	= [attributedString attributedSubstringFromRange:range];
      NSFont *fontInTextStorage;

      [textStorage setAttributedString:attributedSubstring];
      fontInTextStorage = [textStorage attribute:NSFontAttributeName atIndex:0
				       effectiveRange:NULL];
      if ([fontName isEqualToString:[fontInTextStorage fontName]])
	glyphs[i] = [layoutManager glyphAtIndex:0];
      else
	{
	  glyphs[i] = NSNullGlyph;
	  result = false;
	}
      if (range.length == 2)
	glyphs[i + 1] = 0;
      i += range.length;
    }

  [attributedString release];
  [textStorage release];

  return result;
}

@implementation EmacsLocale

/* Initialize the receiver using a given locale identifier.  */

- (id)initWithLocaleIdentifier:(NSString *)string
{
  OSStatus err;
#if MAC_OS_X_VERSION_MAX_ALLOWED == 1040
  FOUNDATION_EXPORT NSString * const NSLocaleExemplarCharacterSet AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER;
#endif

  self = [self init];
  if (self == nil)
    return nil;

  if ([string isEqualToString:@"zh-Hans"])
    string = @"zh_CN";
  else if ([string isEqualToString:@"zh-Hant"])
    string = @"zh_TW";

  err = LocaleStringToLangAndRegionCodes ([string UTF8String],
					  &langCode, &regionCode);
  if (err != noErr)
    {
      [self release];

      return nil;
    }

#if MAC_OS_X_VERSION_MAX_ALLOWED >= 1040
#if MAC_OS_X_VERSION_MIN_REQUIRED < 1040 && MAC_OS_X_VERSION_MIN_REQUIRED >= 1020
  if (NSClassFromString (@"NSLocale"))
#endif
    {
      NSLocale *locale;

#if MAC_OS_X_VERSION_MIN_REQUIRED < 1040 && MAC_OS_X_VERSION_MIN_REQUIRED >= 1020
      locale = [[(NSClassFromString (@"NSLocale")) alloc]
		 initWithLocaleIdentifier:string];
#else
      locale = [[NSLocale alloc] initWithLocaleIdentifier:string];
#endif
      exemplarCharacterSet =
	[[locale objectForKey:NSLocaleExemplarCharacterSet] retain];
      [locale release];
    }
#endif

  return self;
}

#if MAC_OS_X_VERSION_MAX_ALLOWED >= 1040
- (void)dealloc
{
  [exemplarCharacterSet release];
  [super dealloc];
}
#endif

/* Return a Boolean value indicating whether the receiver is
   compatible with the given FONT.  */

- (BOOL)isCompatibleWithFont:(NSFont *)font
{
  OSStatus err;
  NSStringEncoding encoding = [font mostCompatibleStringEncoding];
  CFStringEncoding fontEncoding =
    CFStringConvertNSStringEncodingToEncoding (encoding);
  ScriptCode fontScript;
  LangCode fontLang;
  BOOL result;

  err = GetScriptInfoFromTextEncoding (fontEncoding, &fontScript, &fontLang);
  if (err != noErr)
    result = NO;
  else if (langCode == fontLang)
    result = YES;
  else if (fontLang != kTextLanguageDontCare)
    result = NO;
  else
    {
      TextEncoding textEncoding;

      err = GetTextEncodingFromScriptInfo (fontScript, langCode,
					   regionCode, &textEncoding);
      result = (err == noErr);
    }

#if MAC_OS_X_VERSION_MAX_ALLOWED >= 1040
  if (result == NO)
    if (exemplarCharacterSet
	&& [[font coveredCharacterSet] isSupersetOfSet:exemplarCharacterSet])
      result = YES;
#endif

  return result;
}

@end				// EmacsLocale

@implementation EmacsFontDescriptor

- (id)initWithFontAttributes:(NSDictionary *)attributes
{
  [self doesNotRecognizeSelector:_cmd];
  [self release];

  return nil;
}

+ (id)fontDescriptorWithFontAttributes:(NSDictionary *)attributes
{
  return [[[self alloc] initWithFontAttributes:attributes] autorelease];
}

+ (id)fontDescriptorWithFont:(NSFont *)font
{
  [self doesNotRecognizeSelector:_cmd];

  return nil;
}

- (NSArray *)matchingFontDescriptorsWithMandatoryKeys:(NSSet *)mandatoryKeys
{
  NSMutableArray *locales = nil;
  NSArray *languages = [self objectForKey:FONT_LANGUAGES_ATTRIBUTE];

  if (languages)
    {
      NSEnumerator *enumerator;
      NSString *language;

      locales = [NSMutableArray arrayWithCapacity:[languages count]];
      enumerator = [languages objectEnumerator];
      while ((language = [enumerator nextObject]) != nil)
	{
	  EmacsLocale *locale =
	    [[EmacsLocale alloc] initWithLocaleIdentifier:language];

	  if (locale == nil)
	    break;
	  [locales addObject:locale];
	  [locale release];
	}
      if (language)
	return [NSArray array];
    }

  return [self matchingFontDescriptorsWithMandatoryKeys:mandatoryKeys
	       locales:locales];
}

- (NSArray *)matchingFontDescriptorsWithMandatoryKeys:(NSSet *)mandatoryKeys
					      locales:(NSArray *)locales
{
  [self doesNotRecognizeSelector:_cmd];

  return nil;
}

- (EmacsFontDescriptor *)matchingFontDescriptorWithMandatoryKeys:(NSSet *)mandatoryKeys
{
  NSArray *descriptors =
    [self matchingFontDescriptorsWithMandatoryKeys:mandatoryKeys];

  return [descriptors count] > 0 ? [descriptors objectAtIndex:0] : nil;
}

- (id)objectForKey:(NSString *)anAttribute
{
  [self doesNotRecognizeSelector:_cmd];

  return nil;
}

@end				// EmacsFontDescriptor

#if USE_NS_FONT_DESCRIPTOR

@implementation EmacsFDFontDescriptor

- (id)initWithFontAttributes:(NSDictionary *)attributes
{
  NSFontDescriptor *descriptor;

#if MAC_OS_X_VERSION_MIN_REQUIRED == 1020
  descriptor = [(NSClassFromString (@"NSFontDescriptor"))
		 fontDescriptorWithFontAttributes:attributes];
#else
  descriptor = [NSFontDescriptor fontDescriptorWithFontAttributes:attributes];
#endif

  if (descriptor == nil)
    {
      [self release];

      return nil;
    }

  return [self initWithFontDescriptor:descriptor];
}

- (id)initWithFontDescriptor:(NSFontDescriptor *)aFontDescriptor
{
  self = [self init];
  if (self == nil)
    return nil;

  fontDescriptor = [aFontDescriptor copy];
  if (fontDescriptor == nil)
    {
      [self release];

      return nil;
    }

  return self;
}

- (void)dealloc
{
  [fontDescriptor release];
  [super dealloc];
}

- (NSFontDescriptor *)fontDescriptor
{
  return [[fontDescriptor retain] autorelease];
}

+ (id)fontDescriptorWithFontDescriptor:(NSFontDescriptor *)aFontDescriptor
{
  return [[[self alloc] initWithFontDescriptor:aFontDescriptor] autorelease];
}

+ (id)fontDescriptorWithFont:(NSFont *)font
{
  NSFontDescriptor *descriptor = [font fontDescriptor];
  NSMutableDictionary *attributes =
    [NSMutableDictionary dictionaryWithDictionary:[descriptor fontAttributes]];

  /* On Mac OS 10.4, the above descriptor doesn't contain family or
     size information.  */
  [attributes setObject:[font familyName] forKey:FONT_FAMILY_ATTRIBUTE];
  [attributes setObject:[NSNumber numberWithFloat:[font pointSize]]
	      forKey:FONT_SIZE_ATTRIBUTE];

  return [self fontDescriptorWithFontAttributes:attributes];
}

- (NSArray *)matchingFontDescriptorsWithMandatoryKeys:(NSSet *)mandatoryKeys
{
  if (floor (NSAppKitVersionNumber) <= NSAppKitVersionNumber10_4)
    return [super matchingFontDescriptorsWithMandatoryKeys:mandatoryKeys];
  else
    return [fontDescriptor
	     matchingFontDescriptorsWithMandatoryKeys:mandatoryKeys];
}

- (NSArray *)matchingFontDescriptorsWithMandatoryKeys:(NSSet *)mandatoryKeys
					      locales:(NSArray *)locales
{
  NSFontDescriptor *lastResort = nil;
  NSArray *descriptors;
  NSMutableArray *result;
  NSEnumerator *enumerator;
  NSFontDescriptor *descriptor;

  descriptors = [fontDescriptor
		  matchingFontDescriptorsWithMandatoryKeys:mandatoryKeys];
  result = [NSMutableArray arrayWithCapacity:[descriptors count]];

  enumerator = [descriptors objectEnumerator];
  while ((descriptor = [enumerator nextObject]) != nil)
    {
      NSString *fontName = [descriptor postscriptName];

      if (mac_font_name_is_bogus (fontName))
	continue;

      if (locales)
	{
	  NSFont *font = [NSFont fontWithName:fontName size:0];
	  NSEnumerator *localeEnumerator = [locales objectEnumerator];
	  EmacsLocale *locale;

	  while ((locale = [localeEnumerator nextObject]) != nil)
	    if (![locale isCompatibleWithFont:font])
	      break;
	  if (locale)
	    continue;
	}

      if ([fontName isEqualToString:@"LastResort"])
	{
	  lastResort = descriptor;
	  continue;
	}

      [result addObject:[[self class]
			  fontDescriptorWithFontDescriptor:descriptor]];
    }

  if ([result count] == 0 && lastResort)
    result =
      [NSMutableArray
	arrayWithObject:[[self class]
			  fontDescriptorWithFontDescriptor:lastResort]];

  return result;
}

- (id)objectForKey:(NSString *)anAttribute;
{
  id result = [fontDescriptor objectForKey:anAttribute];

  if ([anAttribute isEqualToString:FONT_TRAITS_ATTRIBUTE])
    {
      NSString *fontName = [fontDescriptor postscriptName];
      NSNumber *weight = mac_font_weight_override_for_name (fontName);

      if (weight)
	{
	  NSMutableDictionary *traits =
	    [NSMutableDictionary dictionaryWithDictionary:result];

	  [traits setObject:weight forKey:FONT_WEIGHT_TRAIT];
	  result = traits;
	}
    }

  return result;
}

@end				// EmacsFDFontDescriptor

#endif	/* USE_NS_FONT_DESCRIPTOR */

#if USE_NS_FONT_MANAGER

@implementation EmacsFMFontDescriptor

- (id)initWithFontAttributes:(NSDictionary *)attributes
{
  self = [self init];
  if (self == nil)
    return nil;

  fontAttributes = [[NSMutableDictionary alloc] initWithDictionary:attributes];
  if (fontAttributes == nil)
    {
      [self release];

      return nil;
    }

  return self;
}

- (void)dealloc
{
  [fontAttributes release];
  [super dealloc];
}

+ (id)fontDescriptorWithFont:(NSFont *)font
{
  NSDictionary *attributes =
    [NSDictionary
      dictionaryWithObjectsAndKeys:[font fontName], FONT_NAME_ATTRIBUTE,
      [NSNumber numberWithFloat:[font pointSize]], FONT_SIZE_ATTRIBUTE,
      [font familyName], FONT_FAMILY_ATTRIBUTE, nil];

  return [self fontDescriptorWithFontAttributes:attributes];
}

- (NSArray *)matchingFontDescriptorsWithMandatoryKeys:(NSSet *)mandatoryKeys
					      locales:(NSArray *)locales
{
  NSString *family = [fontAttributes objectForKey:FONT_FAMILY_ATTRIBUTE];
  NSNumber *size = [fontAttributes objectForKey:FONT_SIZE_ATTRIBUTE];
  NSCharacterSet *charset =
    [fontAttributes objectForKey:FONT_CHARACTER_SET_ATTRIBUTE];
  NSFontManager *fontManager = [NSFontManager sharedFontManager];
  NSArray *fontNamesOrMembers;
  NSMutableArray *result;
  NSEnumerator *enumerator;
  id value;

  if (family == nil)
    fontNamesOrMembers = [fontManager availableFonts];
  else
    fontNamesOrMembers = [fontManager availableMembersOfFontFamily:family];

  result = [NSMutableArray arrayWithCapacity:[fontNamesOrMembers count]];

  enumerator = [fontNamesOrMembers objectEnumerator];
  while ((value = [enumerator nextObject]) != nil)
    {
      NSString *fontName;
      NSFont *font = nil;
      NSDictionary *attributes;

      if (family == nil)
	fontName = (NSString *) value;
      else
	fontName = [(NSArray *)value objectAtIndex:0];

      if (mac_font_name_is_bogus (fontName)
	  || [fontName isEqualToString:@"LastResort"])
	continue;

      if (locales)
	{
	  NSEnumerator *localeEnumerator = [locales objectEnumerator];
	  EmacsLocale *locale;

	  font = [NSFont fontWithName:fontName size:0];

	  while ((locale = [localeEnumerator nextObject]) != nil)
	    if (![locale isCompatibleWithFont:font])
	      break;
	  if (locale)
	    continue;
	}

      if (charset)
	{
	  if (font == nil)
	    font = [NSFont fontWithName:fontName size:0];
	  if (![[font coveredCharacterSet] isSupersetOfSet:charset])
	    continue;
	}

      /* The value of `size' may be nil.  In that case, the
	 variable number of arguments below end there.  */
      if (family || font)
	attributes =
	  [NSDictionary
	    dictionaryWithObjectsAndKeys:fontName, FONT_NAME_ATTRIBUTE,
	    (family ? family : [font familyName]), FONT_FAMILY_ATTRIBUTE,
	    size, FONT_SIZE_ATTRIBUTE, nil];
      else
	attributes =
	  [NSDictionary
	    dictionaryWithObjectsAndKeys:fontName, FONT_NAME_ATTRIBUTE,
	    size, FONT_SIZE_ATTRIBUTE, nil];

      [result addObject:[[self class]
			  fontDescriptorWithFontAttributes:attributes]];
    }

  if (family == nil && [result count] == 0)
    {
      NSDictionary *lastResort =
	[NSDictionary
	  dictionaryWithObjectsAndKeys:@"LastResort", FONT_NAME_ATTRIBUTE,
	  @"LastResort", FONT_FAMILY_ATTRIBUTE, size, FONT_SIZE_ATTRIBUTE, nil];

      [result addObject:[[self class]
			  fontDescriptorWithFontAttributes:lastResort]];
    }

  return result;
}

- (id)objectForKey:(NSString *)anAttribute;
{
  id result = [fontAttributes objectForKey:anAttribute];

  if (result == nil
      && ([anAttribute isEqualToString:FONT_FAMILY_ATTRIBUTE]
	  || [anAttribute isEqualToString:FONT_CHARACTER_SET_ATTRIBUTE]
	  || [anAttribute isEqualToString:FONT_TRAITS_ATTRIBUTE]))
    {
      NSString *fontName = [fontAttributes objectForKey:FONT_NAME_ATTRIBUTE];
      NSFont *font = [NSFont fontWithName:fontName size:0];

      if ([anAttribute isEqualToString:FONT_FAMILY_ATTRIBUTE])
	result = [font familyName];
      else if ([anAttribute isEqualToString:FONT_CHARACTER_SET_ATTRIBUTE])
	result = [font coveredCharacterSet];
      else			/* FONT_TRAITS_ATTRIBUTE */
	if (font)
	  {
	    NSFontManager *fontManager = [NSFontManager sharedFontManager];
	    NSFontTraitMask traits = [fontManager traitsOfFont:font];
	    FontSymbolicTraits symbolicTraits =
	      (traits & (MAC_FONT_ITALIC_TRAIT | MAC_FONT_BOLD_TRAIT
			 | MAC_FONT_MONO_SPACE_TRAIT));
	    NSNumber *weight = mac_font_weight_override_for_name (fontName);
	    float width = ((traits & NSCondensedFontMask) ? -0.2f
			   : (traits & NSExpandedFontMask) ? 0.2f : 0);
	    float slant = ((traits & NSItalicFontMask)
			   ? [font italicAngle] / -18.0f : 0);

	    if (weight == nil)
	      {
		float value = ([fontManager weightOfFont:font] - 5) * 0.1f;

		weight = [NSNumber numberWithFloat:value];
	      }

	    result =
	      [NSDictionary
		dictionaryWithObjectsAndKeys:[NSNumber numberWithInt:symbolicTraits], FONT_SYMBOLIC_TRAIT,
		weight, FONT_WEIGHT_TRAIT,
		[NSNumber numberWithFloat:width], FONT_WIDTH_TRAIT,
		[NSNumber numberWithFloat:slant], FONT_SLANT_TRAIT, nil];
	  }
      if (result)
	[fontAttributes setObject:result forKey:anAttribute];
    }

  return result;
}

@end				// EmacsFMFontDescriptor

#endif	/* USE_NS_FONT_MANAGER */

FontDescriptorRef
mac_font_descriptor_create_with_attributes (attributes)
     CFDictionaryRef attributes;
{
  EmacsFontDescriptor *result;

#if USE_CORE_TEXT
  if (EQ (macfont_driver_type, Qmac_ct))
    return ((FontDescriptorRef)
	    CTFontDescriptorCreateWithAttributes (attributes));
#endif
#if USE_NS_FONT_DESCRIPTOR
#if USE_NS_FONT_MANAGER
  if (EQ (macfont_driver_type, Qmac_fd))
#endif
    {
      result =
	[EmacsFDFontDescriptor
	  fontDescriptorWithFontAttributes:((NSDictionary *) attributes)];
    }
#if USE_NS_FONT_MANAGER
  else
#endif
#endif
#if USE_NS_FONT_MANAGER
    {
      result =
	[EmacsFMFontDescriptor
	  fontDescriptorWithFontAttributes:((NSDictionary *) attributes)];
    }
#endif

  return result ? CFRetain (result) : NULL;
}

CFArrayRef
mac_font_descriptor_create_matching_font_descriptors (descriptor,
						      mandatoryAttributes)
     FontDescriptorRef descriptor;
     CFSetRef mandatoryAttributes;
{
  EmacsFontDescriptor *fontDescriptor = (EmacsFontDescriptor *) descriptor;
  NSSet *mandatoryKeys = (NSSet *) mandatoryAttributes;
  NSArray *result =
    [fontDescriptor matchingFontDescriptorsWithMandatoryKeys:mandatoryKeys];

  return result ? CFRetain (result) : NULL;
}

FontDescriptorRef
mac_font_descriptor_create_matching_font_descriptor (descriptor,
						     mandatoryAttributes)
     FontDescriptorRef descriptor;
     CFSetRef mandatoryAttributes;
{
  EmacsFontDescriptor *fontDescriptor = (EmacsFontDescriptor *) descriptor;
  NSSet *mandatoryKeys = (NSSet *) mandatoryAttributes;
  EmacsFontDescriptor *result =
    [fontDescriptor matchingFontDescriptorWithMandatoryKeys:mandatoryKeys];

  return result ? CFRetain (result) : NULL;
}

CFTypeRef
mac_font_descriptor_copy_attribute (descriptor, attribute)
     FontDescriptorRef descriptor;
     CFStringRef attribute;
{
  EmacsFontDescriptor *fontDescriptor = (EmacsFontDescriptor *) descriptor;
  id result = [fontDescriptor objectForKey:((NSString *) attribute)];

  return result ? CFRetain (result) : NULL;
}

Boolean
mac_font_descriptor_supports_languages (descriptor, languages)
     FontDescriptorRef descriptor;
     CFArrayRef languages;
{
#if USE_CORE_TEXT
  if (EQ (macfont_driver_type, Qmac_ct))
    return mac_ctfont_descriptor_supports_languages (((CTFontDescriptorRef)
						      descriptor), languages);
#endif
  {
    Boolean result = true;
    NSString *name =
      [(EmacsFontDescriptor *)descriptor objectForKey:FONT_NAME_ATTRIBUTE];
    NSFont *font = [NSFont fontWithName:name size:0];

    if (font == nil)
      result = false;
    else
      {
	NSEnumerator *enumerator;
	NSString *language;

	enumerator = [(NSArray *)languages objectEnumerator];
	while ((language = [enumerator nextObject]) != nil)
	  {
	    EmacsLocale *locale =
	      [[EmacsLocale alloc] initWithLocaleIdentifier:language];
	    BOOL isCompatible = [locale isCompatibleWithFont:font];

	    [locale release];
	    if (!isCompatible)
	      {
		result = false;
		break;
	      }
	  }
      }

    return result;
  }
}

FontRef
mac_font_create_with_name (name, size)
     CFStringRef name;
     CGFloat size;
{
  NSFont *result;
  NSFont *font;

#if USE_CORE_TEXT
  if (EQ (macfont_driver_type, Qmac_ct))
    return (FontRef) CTFontCreateWithName (name, size, NULL);
#endif
  font = [NSFont fontWithName:((NSString *) name) size:size];
  result = (NSFont *) font;

  return result ? CFRetain (result) : NULL;
}

CGFloat
mac_font_get_size (font)
     FontRef font;
{
  return [(NSFont *)font pointSize];
}

CFStringRef
mac_font_copy_family_name (font)
     FontRef font;
{
  return CFRetain ([(NSFont *)font familyName]);
}

CFCharacterSetRef
mac_font_copy_character_set (font)
     FontRef font;
{
  return CFRetain ([(NSFont *)font coveredCharacterSet]);
}

Boolean
mac_font_get_glyphs_for_characters (font, characters, glyphs, count)
     FontRef font;
     const UniChar characters[];
     CGGlyph glyphs[];
     CFIndex count;
{
#if USE_CORE_TEXT
  if (EQ (macfont_driver_type, Qmac_ct))
    return CTFontGetGlyphsForCharacters ((CTFontRef) font, characters,
					 glyphs, count);
#endif
#if USE_NS_FONT_DESCRIPTOR
#if USE_NS_FONT_MANAGER
  if (EQ (macfont_driver_type, Qmac_fd))
#endif
    {
#if MAC_OS_X_VERSION_MAX_ALLOWED >= 1050
      return CTFontGetGlyphsForCharacters ((CTFontRef) font, characters,
					   glyphs, count);
#else
      extern Boolean CTFontGetGlyphsForCharacters P_ ((const void *,
						       const UniChar [],
						       CGGlyph glyphs [],
						       CFIndex)) AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER;

      return CTFontGetGlyphsForCharacters (font, characters, glyphs, count);
#endif
    }
#if USE_NS_FONT_MANAGER
  else
#endif
#endif
#if USE_NS_FONT_MANAGER
    {
      return get_glyphs_for_characters ((NSFont *) font, characters,
					glyphs, count);
    }
#endif
}

CGFloat
mac_font_get_ascent (font)
     FontRef font;
{
  return [(NSFont *)font ascender];
}

CGFloat
mac_font_get_descent (font)
     FontRef font;
{
  return - [(NSFont *)font descender];
}

CGFloat
mac_font_get_leading (font)
     FontRef font;
{
  NSFont *nsFont = (NSFont *) font;

#if MAC_OS_X_VERSION_MAX_ALLOWED >= 1040
#if MAC_OS_X_VERSION_MIN_REQUIRED < 1040
  if ([nsFont respondsToSelector:@selector(leading)])
#endif
    return [nsFont leading];
#if MAC_OS_X_VERSION_MIN_REQUIRED < 1040
  else
#endif
#endif
#if MAC_OS_X_VERSION_MIN_REQUIRED < 1040
    return ([nsFont defaultLineHeightForFont]
	    - ([nsFont ascender] - [nsFont descender]));
#endif
}

CGFloat
mac_font_get_underline_position (font)
     FontRef font;
{
  return [(NSFont *)font underlinePosition];
}

CGFloat
mac_font_get_underline_thickness (font)
     FontRef font;
{
  return [(NSFont *)font underlineThickness];
}

CGFloat
mac_font_get_advance_width_for_glyph (font, glyph)
     FontRef font;
     CGGlyph glyph;
{
  NSSize advancement = [(NSFont *)font advancementForGlyph:glyph];

  return advancement.width;
}

CFStringRef
mac_font_create_preferred_family_for_attributes (attributes)
     CFDictionaryRef attributes;
{
#if USE_CORE_TEXT
  if (EQ (macfont_driver_type, Qmac_ct))
    return mac_ctfont_create_preferred_family_for_attributes (attributes);
#endif
  {
    NSString *result = nil;
    CFStringRef charsetString =
      CFDictionaryGetValue (attributes,
			    MAC_FONT_CHARACTER_SET_STRING_ATTRIBUTE);
    CFIndex length;

    if (charsetString
	&& (length = CFStringGetLength (charsetString)) > 0)
      {
	NSMutableAttributedString *attrString =
	  [[[NSMutableAttributedString alloc]
	     initWithString:((NSString *) charsetString)] autorelease];
	NSRange attrStringRange = NSMakeRange(0, [attrString length]), range;
	NSFont *font;

	[attrString fixFontAttributeInRange:attrStringRange];
	font = [attrString attribute:NSFontAttributeName atIndex:0
			   longestEffectiveRange:&range
			   inRange:attrStringRange];
	if (NSEqualRanges (range, attrStringRange))
	  {
	    result = [font familyName];
	    if ([result isEqualToString:@"LastResort"])
	      result = nil;
	  }
      }

    return result ? CFRetain (result) : result;
  }
}

CGRect
mac_font_get_bounding_rect_for_glyph (font, glyph)
     FontRef font;
     CGGlyph glyph;
{
  NSRect rect = [(NSFont *)font boundingRectForGlyph:glyph];

  return NSRectToCGRect (rect);
}

CGFontRef
mac_font_copy_graphics_font (font)
     FontRef font;
{
#if USE_CORE_TEXT
  if (EQ (macfont_driver_type, Qmac_ct))
    return CTFontCopyGraphicsFont ((CTFontRef) font, NULL);
#endif
  {
    ATSFontRef atsfont =
      ATSFontFindFromPostScriptName ((CFStringRef) [(NSFont *)font fontName],
				     kATSOptionFlagsDefault);

    return CGFontCreateWithPlatformFont (&atsfont);
  }
}

CFArrayRef
mac_font_create_available_families ()
{
  NSArray *families = [[NSFontManager sharedFontManager] availableFontFamilies];
  CFIndex count = [families count];
  CFMutableArrayRef result =
    CFArrayCreateMutableCopy (NULL, count, (CFArrayRef) families);

  while (count-- > 0)
    if (CFStringHasPrefix (CFArrayGetValueAtIndex (result, count),
			   CFSTR (".")))
      CFArrayRemoveValueAtIndex (result, count);

  CFArraySortValues (result, CFRangeMake (0, CFArrayGetCount (result)),
		     mac_font_family_compare, NULL);

  return result;
}

FontDescriptorRef
mac_nsctfont_copy_font_descriptor (font)
     void *font;
{
#if USE_CORE_TEXT
  if (EQ (macfont_driver_type, Qmac_ct))
    return (FontDescriptorRef) CTFontCopyFontDescriptor ((CTFontRef) font);
#endif
  {
    EmacsFontDescriptor *result;
    NSFont *nsFont = (NSFont *) font;

#if USE_NS_FONT_DESCRIPTOR
#if USE_NS_FONT_MANAGER
    if (EQ (macfont_driver_type, Qmac_fd))
#endif
      {
	result = [EmacsFDFontDescriptor fontDescriptorWithFont:nsFont];
      }
#if USE_NS_FONT_MANAGER
    else
#endif
#endif
#if USE_NS_FONT_MANAGER
      {
	result = [EmacsFMFontDescriptor fontDescriptorWithFont:nsFont];
      }
#endif

    return result ? CFRetain (result) : NULL;
  }
}

CFIndex
mac_font_shape (font, string, glyph_layouts, glyph_len)
     FontRef font;
     CFStringRef string;
     struct mac_glyph_layout *glyph_layouts;
     CFIndex glyph_len;
{
  NSUInteger i;
  CFIndex result = 0;
  NSFont *nsFont;
  NSTextStorage *textStorage;
  NSLayoutManager *layoutManager;
  NSTextContainer *textContainer;
  NSUInteger stringLength;
  NSPoint spaceLocation;
  NSUInteger used, numberOfGlyphs;

#if USE_CORE_TEXT
  if (EQ (macfont_driver_type, Qmac_ct))
    return mac_ctfont_shape ((CTFontRef) font, string,
			     glyph_layouts, glyph_len);
#endif
  nsFont = (NSFont *) font;

  textStorage = [[NSTextStorage alloc] initWithString:((NSString *) string)];
  layoutManager = [[NSLayoutManager alloc] init];
  textContainer = [[NSTextContainer alloc] init];

  /* Append a trailing space to measure baseline position.  */
  [textStorage appendAttributedString:[[[NSAttributedString alloc]
					 initWithString:@" "] autorelease]];
  [textStorage setFont:nsFont];
  [textContainer setLineFragmentPadding:0];

  [layoutManager addTextContainer:textContainer];
  [textContainer release];
  [textStorage addLayoutManager:layoutManager];
  [layoutManager release];

  if (!(textStorage && layoutManager && textContainer))
    {
      [textStorage release];

      return 0;
    }

  stringLength = CFStringGetLength (string);

  /* Force layout.  */
  (void) [layoutManager glyphRangeForTextContainer:textContainer];

  spaceLocation = [layoutManager locationForGlyphAtIndex:stringLength];

  {
    NSRange range = NSMakeRange (0, stringLength);

    range = [layoutManager glyphRangeForCharacterRange:range
			   actualCharacterRange:NULL];
    numberOfGlyphs = NSMaxRange (range);
  }

  // XXX: Check fonts in textStorage.

  used = numberOfGlyphs;
  for (i = 0; i < numberOfGlyphs; i++)
    if ([layoutManager notShownAttributeForGlyphAtIndex:i])
      used--;

  if (used <= glyph_len)
    {
      NSUInteger glyphIndex = 0;
      NSRange compRange = NSMakeRange (0, 0);
      CGFloat totalAdvance = 0;

      while ([layoutManager notShownAttributeForGlyphAtIndex:glyphIndex])
	glyphIndex++;

      for (i = 0; i < used; i++)
	{
	  struct mac_glyph_layout *gl = glyph_layouts + i;
	  NSUInteger characterIndex;
	  NSPoint location;
	  NSRect *glyphRects;
	  NSUInteger nrects;

	  characterIndex = [layoutManager
			     characterIndexForGlyphAtIndex:glyphIndex];
	  if (characterIndex >= NSMaxRange (compRange))
	    {
	      compRange.location = NSMaxRange (compRange);
	      compRange.length =
		(NSMaxRange
		 ([((NSString *) string)
		    rangeOfComposedCharacterSequenceAtIndex:characterIndex])
		 - compRange.location);
	    }

	  gl->comp_range.location = compRange.location;
	  gl->comp_range.length = compRange.length;
	  gl->glyph_id = [layoutManager glyphAtIndex:glyphIndex];

	  location = [layoutManager locationForGlyphAtIndex:glyphIndex];
	  gl->advance_delta = location.x - totalAdvance;
	  gl->baseline_delta = spaceLocation.y - location.y;

	  while (glyphIndex + 1 < numberOfGlyphs
		 && [layoutManager
		      notShownAttributeForGlyphAtIndex:(glyphIndex + 1)])
	    glyphIndex++;
	  glyphRects = [layoutManager
			 rectArrayForGlyphRange:(NSMakeRange (glyphIndex, 1))
			 withinSelectedGlyphRange:(NSMakeRange (NSNotFound, 0))
			 inTextContainer:textContainer rectCount:&nrects];
	  gl->advance = NSMaxX (glyphRects[0]) - totalAdvance;
	  totalAdvance = NSMaxX (glyphRects[0]);

	  glyphIndex++;
	}
      result = used;
    }
  [textStorage release];

  return result;
}

#endif	/* MAC_OS_X_VERSION_MIN_REQUIRED < 1050 */
