/* Definitions and headers for AppKit framework on the Mac OS.
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

#undef Z
#import <Cocoa/Cocoa.h>
#define Z (current_buffer->text->z)

#ifndef NSAppKitVersionNumber10_3
#define NSAppKitVersionNumber10_3 743
#endif
#ifndef NSAppKitVersionNumber10_4
#define NSAppKitVersionNumber10_4 824
#endif

#ifndef NSINTEGER_DEFINED
typedef int NSInteger;
typedef unsigned int NSUInteger;
#endif

@interface NSData (Emacs)
- (Lisp_Object)lispString;
@end

@interface NSString (Emacs)
+ (id)stringWithLispString:(Lisp_Object)lispString;
+ (id)stringWithUTF8String:(const char *)bytes fallback:(BOOL)flag;
- (Lisp_Object)lispString;
- (Lisp_Object)UTF8LispString;
- (Lisp_Object)UTF16LispString;
@end

@interface NSFont (Emacs)
+ (NSFont *)fontWithFace:(struct face *)face;
@end

@interface NSEvent (Emacs)
- (NSEvent *)mouseEventByChangingType:(NSEventType)type
		          andLocation:(NSPoint)location;
@end

@interface NSAttributedString (Emacs)
- (Lisp_Object)UTF16LispString;
@end

@interface NSImage (Emacs)
+ (id)imageWithCGImage:(CGImageRef)cgImage;
@end

@interface NSApplication (Emacs)
- (void)postDummyEvent;
- (void)runTemporarilyWithInvocation:(NSInvocation *)invocation;
@end

@interface EmacsApplication : NSApplication
@end

@interface EmacsPosingWindow : NSWindow
+ (void)setup;
@end

/* Class for delegate for NSApplication.  It also becomes the target
   of several actions such as those from EmacsView, menus, dialogs,
   and actions/services bound in the mac-apple-event keymap.  */

@interface EmacsController : NSObject
{
  /* Points to HOLD_QUIT arg passed to read_socket_hook.  */
  struct input_event *hold_quit;

  /* Number of events stored during a
     handleQueuedEventsWithHoldingQuitIn: call.  */
  int count;

  /* Whether to generate a HELP_EVENT at the end of handleOneNSEvent:
     call.  */
  int do_help;

  /* Non-zero means that a HELP_EVENT has been generated since Emacs
   start.  */
  int any_help_event_p;

  /* The frame on which a HELP_EVENT occurs.  */
  struct frame *emacsHelpFrame;

  /* Non-nil means left mouse tracking has been suspended by this
     object.  */
  id trackingObject;

  /* Selector used for resuming suspended left mouse tracking.  */
  SEL trackingResumeSelector;
}
- (void)storeInputEvent:(id)sender;
- (void)setMenuItemSelectionToTag:(id)sender;
- (void)storeEvent:(struct input_event *)bufp;
- (void)setTrackingObject:(id)object andResumeSelector:(SEL)selector;
- (int)handleQueuedNSEventsWithHoldingQuitIn:(struct input_event *)bufp;
@end

/* Like NSWindow, but allows suspend/resume resize control tracking.  */

@interface EmacsWindow : NSWindow
{
  /* Left mouse up event used for suspending resize control
     tracking.  */
  NSEvent *mouseUpEvent;

  /* Offset of mouse position for the event that initiated mouse
     tracking.  */
  NSPoint resizeTrackingOffset;

  /* Whether window's resize control needs display.  */
  BOOL resizeControlNeedsDisplay;

  /* Whether the window should be made visible when the application
     gets unhidden next time.  */
  BOOL needsOrderFrontOnUnhide;
}
- (void)suspendResizeTracking:(NSEvent *)event;
- (void)resumeResizeTracking;
- (BOOL)resizeControlNeedsDisplay;
- (void)setResizeControlNeedsDisplay:(BOOL)flag;
- (void)displayResizeControlIfNeeded;
- (BOOL)needsOrderFrontOnUnhide;
- (void)setNeedsOrderFrontOnUnhide:(BOOL)flag;
@end

/* Class for delegate of NSWindow and NSToolbar (see its Toolbar
   category declared later).  It also becomes that target of
   frame-dependent actions such as those from font panels.  */

@interface EmacsFrameController : NSObject
{
  /* The Emacs frame corresponding to the NSWindow that
     EmacsFrameController object is associated with as delegate.  */
  struct frame *emacsFrame;
}
- (id)initWithEmacsFrame:(struct frame *)emacsFrame;
- (struct frame *)emacsFrame;
@end

/* Class for Emacs view that handles drawing events only.  It is used
   directly by tooltip frames, and indirectly by ordinary frames via
   inheritance.  */

@interface EmacsTipView : NSView
- (struct frame *)emacsFrame;
@end

/* Class for Emacs view that also handles input events.  Used by
   ordinary frames.  */

@interface EmacsView : EmacsTipView <NSTextInput>
{
  /* Target object to which the EmacsView object sends actions.  */
  id target;

  /* Message selector of the action the EmacsView object sends.  */
  SEL action;

  /* Stores the Emacs input event that the action method is expected
     to process.  */
  struct input_event inputEvent;

  /* Whether key events were interpreted by intepretKeyEvents:.  */
  BOOL keyEventsInterpreted;

  /* Raw key event that is interpreted by intepretKeyEvents:.  */
  NSEvent *rawKeyEvent;

  /* Saved marked text passed by setMarkedText:selectedRange:.  */
  id markedText;
}
- (id)target;
- (SEL)action;
- (void)setTarget:(id)anObject;
- (void)setAction:(SEL)aSelector;
- (BOOL)sendAction:(SEL)theAction to:(id)theTarget;
- (struct input_event *)inputEvent;
- (void)viewFrameDidChange:(NSNotification *)aNotification;
@end

/* Class for scroller that doesn't do modal mouse tracking.  */

@interface NonmodalScroller : NSScroller
{
  /* Timer used for posting events periodically during mouse
     tracking.  */
  NSTimer *timer;

  /* Code for the scroller part that the user hit.  */
  NSScrollerPart hitPart;

  /* Whether the hitPart area should be highlighted.  */
  BOOL hilightsHitPart;

  /* If the scroller knob is currently being dragged by the user, this
     is the number of pixels from the top of the knob to the place
     where the user grabbed it.  If the knob is pressed but not
     dragged yet, this is a negative number whose absolute value is
     the number of pixels plus 1.  */
  CGFloat knobGrabOffset;

  /* The position of the top (for vertical scroller) or left (for
     horizontal, respectively) of the scroller knob in pixels,
     relative to the knob slot.  */
  CGFloat knobMinEdgeInSlot;
}
- (BOOL)dragUpdatesFloatValue;
- (NSTimeInterval)firstDelay;
- (NSTimeInterval)continuousDelay;
@end

/* Just for avoiding warnings about undocumented methods in NSScroller.  */

@interface NSScroller (Emacs)
- (void)drawArrow:(NSUInteger)position highlightPart:(NSInteger)part;
@end

/* Class for Scroller used for an Emacs window.  */

@interface EmacsScroller : NonmodalScroller
{
  /* Emacs scroll bar for the scroller.  */
  struct scroll_bar *emacsScrollBar;

  /* The size of the scroller knob track area in pixels.  */
  CGFloat knobSlotSpan;

  /* Minimum size of the scroller knob, in pixels.  */
  CGFloat minKnobSpan;

  /* The size of the whole scroller area in pixels.  */
  CGFloat frameSpan;

  /* The position the user clicked in pixels, relative to the whole
     scroller area.  */
  CGFloat clickPositionInFrame;

  /* For a scroller click with the control modifier, this becomes the
     value of the `code' member in struct input_event.  */
  int inputEventCode;

  /* For a scroller click with the control modifier, this becomes the
     value of the `modifiers' member in struct input_event.  */
  int inputEventModifiers;
}
- (void)setEmacsScrollBar:(struct scroll_bar *)bar;
- (struct scroll_bar *)emacsScrollBar;
- (CGFloat)knobSlotSpan;
- (CGFloat)minKnobSpan;
- (CGFloat)knobMinEdgeInSlot;
- (CGFloat)frameSpan;
- (CGFloat)clickPositionInFrame;
- (int)inputEventCode;
- (int)inputEventModifiers;
@end

@interface EmacsToolbarItem : NSToolbarItem
{
  /* CoreGraphics image of the item.  */
  CGImageRef coreGraphicsImage;
}
- (void)setCoreGraphicsImage:(CGImageRef)cgImage;
@end

@interface EmacsFrameController (Toolbar)
- (NSToolbarItem *)toolbar:(NSToolbar *)toolbar
     itemForItemIdentifier:(NSString *)itemIdentifier
 willBeInsertedIntoToolbar:(BOOL)flag;
- (NSArray *)toolbarAllowedItemIdentifiers:(NSToolbar *)toolbar;
- (NSArray *)toolbarDefaultItemIdentifiers:(NSToolbar *)toolbar;
- (BOOL)validateToolbarItem:(NSToolbarItem *)theItem;
- (void)storeToolBarEvent:(id)sender;
@end

/* Like NSFontPanel, but allows suspend/resume slider tracking.  */

@interface EmacsFontPanel : NSFontPanel
{
  /* Left mouse up event used for suspending slider tracking.  */
  NSEvent *mouseUpEvent;

  /* Slider being tracked.  */
  NSSlider *trackedSlider;
}
- (void)suspendSliderTracking:(NSEvent *)event;
- (void)resumeSliderTracking;
@end

@interface EmacsController (FontPanel)
- (void)fontPanelWillClose:(NSNotification *)aNotification;
@end

@interface EmacsFrameController (FontPanel)
- (NSFont *)fontForFace:(int)faceId character:(int)c
	       position:(int)pos object:(Lisp_Object)object;
- (void)changeFont:(id)sender;
@end

@interface EmacsSavePanel : NSSavePanel
@end

@interface EmacsOpenPanel : NSOpenPanel
@end

@interface EmacsFontDialogController : NSObject
@end

@interface NSFontPanel (Emacs)
- (NSInteger)runModal;
@end

@interface NSMenu (Emacs)
- (NSMenuItem *)addItemWithWidgetValue:(widget_value *)wv;
- (void)fillWithWidgetValue:(widget_value *)first_wv;
@end

@interface EmacsMenu : NSMenu
@end

@interface EmacsController (Menu)
- (void)trackMenubar;
@end

@interface EmacsDialogView : NSView
- (id)initWithWidgetValue:(widget_value *)wv;
@end

@interface NSPasteboard (Emacs)
- (BOOL)setLispObject:(Lisp_Object)lispObject forType:(NSString *)dataType;
- (Lisp_Object)lispObjectForType:(NSString *)dataType;
@end

@interface NSAppleEventDescriptor (Emacs)
- (OSErr)copyDescTo:(AEDesc *)desc;
@end

@interface EmacsController (AppleScript)
- (long)doAppleScript:(Lisp_Object)script result:(Lisp_Object *)result;
@end

#if MAC_OS_X_VERSION_MIN_REQUIRED < 1050

/* Class for locale objects used in kCTFontLanguagesAttribute
   emulation.  */

@interface EmacsLocale : NSObject
{
  /* Mac OS language and region codes for the locale.  */
  LangCode langCode;
  RegionCode regionCode;

#if MAC_OS_X_VERSION_MAX_ALLOWED >= 1040
  /* Exemplar character set for the locale.  */
  NSCharacterSet *exemplarCharacterSet;
#endif
}
- (id)initWithLocaleIdentifier:(NSString *)string;
- (BOOL)isCompatibleWithFont:(NSFont *)font;
@end

/* Class for CTFontDescriptor replacement for < 10.5 systems.  Some
   selectors are compatible with those for NSFontDescriptor, so
   toll-free bridged CTFontDescriptor can also respond to them.
   Implementations of some methods are dummy and each subclass
   (EmacsFDFontDescriptor or EmacsFMFontDescriptor below) should
   override them.  */

@interface EmacsFontDescriptor : NSObject
- (id)initWithFontAttributes:(NSDictionary *)attributes;
+ (id)fontDescriptorWithFontAttributes:(NSDictionary *)attributes;
+ (id)fontDescriptorWithFont:(NSFont *)font;
- (NSArray *)matchingFontDescriptorsWithMandatoryKeys:(NSSet *)mandatoryKeys;
- (NSArray *)matchingFontDescriptorsWithMandatoryKeys:(NSSet *)mandatoryKeys
					      locales:(NSArray *)locales;
- (EmacsFontDescriptor *)matchingFontDescriptorWithMandatoryKeys:(NSSet *)mandatoryKeys;
- (id)objectForKey:(NSString *)anAttribute;
@end

#if USE_NS_FONT_DESCRIPTOR
@interface EmacsFDFontDescriptor : EmacsFontDescriptor
{
  NSFontDescriptor *fontDescriptor;
}
- (id)initWithFontDescriptor:(NSFontDescriptor *)aFontDescriptor;
- (NSFontDescriptor *)fontDescriptor;
+ (id)fontDescriptorWithFontDescriptor:(NSFontDescriptor *)aFontDescriptor;
@end

#endif

#if USE_NS_FONT_MANAGER
@interface EmacsFMFontDescriptor : EmacsFontDescriptor
{
  NSMutableDictionary *fontAttributes;
}
@end

#endif

#endif	/* MAC_OS_X_VERSION_MIN_REQUIRED < 1050 */

/* Some methods that are not declared in older versions.  Should be
   used with some runtime check such as `respondsToSelector:'. */

#if MAC_OS_X_VERSION_MAX_ALLOWED < 1040
@interface NSWindow (AvailableOn1040AndLater)
- (CGFloat)userSpaceScaleFactor;
@end
#endif

#if MAC_OS_X_VERSION_MAX_ALLOWED < 1030
@interface NSSavePanel (AvailableOn1030AndLater)
- (void)setNameFieldLabel:(NSString *)label;
@end
#endif

#if MAC_OS_X_VERSION_MAX_ALLOWED < 1030
@interface NSMenu (AvailableOn1030AndLater)
- (void)setDelegate:(id)anObject;
@end
#endif
