      *set p(prexml) endp
       copy "windows.cpy".
      $set sourceformat"variable"
       identification division.
       program-id. custommenu.
      ********************************************************************
      *
      *    Author - David Sands - Micro Focus EMEA Technical Support
      *
      *    email  - david.sands@microfocus.com
      *
      *
      *    This routine will mimic the menus as seen in Office XP and
      *    Office 2003.
      *
      *    It doesn't do everything Office menus do but gives an basic
      *    look and feel to your Dialog System Menus.
      *
      *    It use owner draw to take responsibility for painting menus
      *    from Windows. On the WINDOW-CREATE event this routine should
      *    called (before the window is shown) and it should be passed
      *    the Windows handle for the window that you the menus to be
      *    painted for and a flag to tell this module the Window has been
      *    created. This module will then subclass the window to recieve
      *    native windows messages before Dialog and Windows receives them.
      *
      *    The program initially loads the ICONS from files and adds an Owner
      *    draw flag to all the menu items on the window. This is done
      *    recursively down the menu structure.
      *
      *    It intercepts and processes 2 messages before Dialog gets them.
      *
      *        WM_MEASUREITEM  - This is the system requesting the size
      *                          of this menu item (height and width).
      *
      *
      *        WM_DRAWITEM     - This is the system requesting you to paint
      *                          a menu item.
      *
      *    All other messages and notifications are passed directly through
      *    to the old Dialog System winproc.
      *
      *    Before the Window is destroyed the application must remove the
      *    subclass therefore on the WINDOW-DELETED event you need to
      *    call this program and tell it that you no longer need the
      *    window subclassed.
      *
      *    This example also handles the drawing of ICONS on a Menu.
      *
      ********************************************************************
       class-control.
      *    TextHeader is class "textheader"
      *> OCWIZARD - start list of classes
      *    WINDOW is class "window"
      *> OCWIZARD - end list of classes
      *>---USER-CODE. Add any additional class names below.
      *    CharacterArray is class "chararry"
      *    font is class "font"
      *    SysInformation is class "sysinfo"
      *    color is class "color"
      *    brush is class "brush"
      *    pen is class "pen"
           Module is class "module"
      *    iconData is class "icondata"
      *    MaskedImageList is class "mskimglt"
      *    bitmapData is class "bitmapd"
           .
       special-names.
           call-convention 74 is winapi.

      *select xml-stream assign "out.xml"
      *      organization  is xml
      *      document-type is "group"
      *      file status is xml-bookdb-status.

       working-storage section.
       copy "mfres.cpy".
      ************************************************************************
      *  CONSTANTS for the menu handling
      ************************************************************************
       78  icon-offset-x           value 4.
       78  max-menu-items          value 1000.
       78  max-menu-string         value 100.
       78  max-active-windows      value 15.

       01  ws-first-timeflag       pic 9   value 0.
           88  first-time-in               value 0.
           88  not-first-time-in           value 1.

       01  menu-details occurs max-menu-items.
           03  menu-str            pic x(max-menu-string).
           03  menu-str-len        pic 9(9) comp-5 value 0.
           03  menu-icon           HICON.
           03  menu-ftype          UINT.
       01  menu-itemcount          pic 9(9) comp-5 value 0.

       01  retval                  BOOL.
       01  callback-retval         BOOL.
       01  mywinproc               procedure-pointer.
       01  oldwinproc              procedure-pointer.
       01  ws-retcode              long redefines oldwinproc.
       01  App-mResult             DWORD.

       01  ws-proc                 procedure-pointer.

       01  anHMENU                 HMENU.
       01  asubHMENU               HMENU.
       01  wx-menu                 int.

       78  MIIM-FTYPE          value h"00000100".
       78  MIIM-STRING         value h"00000040".

       01  ws-uintptr              uint.
       01  ws-myptr redefines ws-uintptr pointer.

       01  ws-str          pic x(50).
       01  ws-format               uint.
       01  ws-rect                 rect.


       01 ws-grad-fromRGB  COLORREF.
       01 ws-grad-toRGB    COLORREF.

       copy "rgbhsl.cpy" replacing ==:TAG:== by ==ws-==.

       01 ws-text-col      COLORREF.
       01 ws-colour-grey   COLORREF.
       01 ws-rgb           pic 9(9) COMP-5.
       01 ws-rgb-redef     redefines ws-rgb.
          03  ws-rgb-red   pic x    comp-5.
          03  ws-rgb-green pic x    comp-5.
          03  ws-rgb-blue  pic x    comp-5.
          03  filler       pic x.

       78  GRADIENT-FILL-RECT-H    value h"00000000".
       78  GRADIENT-FILL-RECT-V    value h"00000001".
       78  GRADIENT-FILL-TRIANGLE  value h"00000002".
       78  GRADIENT-FILL-OP-FLAG   value h"000000ff".

       01 COLOr16          ushort  typedef.

       01 TRIVERTEX   typedef.
          03 x        LONG.
          03 y        LONG.
          03 red      COLOR16.
          03 Green    COLOR16.
          03 Blue     COLOR16.
          03 Alpha    COLOR16.

       01 dwNumVertex     ulong.
       01 dwNumMesh       ulong.
       01 dwMode          ulong.
       01 ws-retval       BOOL.
       01 WS-TRIVERTEXArray.
          03  ws-TRIVERTEX TRIVERTEX OCCURS 2.

       01 GRADIENT-RECT   typedef.
          03  UpperLeft   ULONG.
          03  LowerRight  ULONG.

       01 ws-mesharray.
          03  ws-mesh   GRADIENT-RECT occurs 1.

       01 ws-bittest      pic s9(9) COMP-5.
       01 ws-result       pic s9(9) COMP-5.

       01 ws-drawflag     pic 9.
           88  FLAG-ODA-DRAWENTIRE    VALUE 1.
           88  FLAG-ODA-SELECT        VALUE 2.
           88  FLAG-ODA-FOCUS         VALUE 3.

       01 ws-stateflag     pic 9.
           88  FLAG-DISABLE           VALUE 1.
           88  FLAG-ENABLED           VALUE 2.

      *01 ws-menu-color           pic 9(9) COMP-5.

       01 ws-old-pen              hpen.
       01 ws-base-pen             hpen.
      *01 ws-select-pen           hpen.
       01 ws-old-brush            hbrush.
       01 ws-base-brush           hbrush.
      *01 ws-select-brush         hbrush.
       01 ws-y                        uint.
       01 ws-x                        uint.

       01 ws-hdc                   HDC.
       01 ws-size                  tagSIZE.
       01 ws-font                  HFONT.
       01 ws-old-font              HFONT.
       01 ws-resource              object reference.
      *01 ws-disk                  object reference.
      *01 ws-face1                 object reference.
      *01 ws-face2                 object reference.
      *01 ws-question              object reference.
      *01 ws-radio                 object reference.
       01 ws-ResourceID            pic x(4) comp-5.
       01 ws-hdisk                 HICON.
       01 ws-hface1                HICON.
       01 ws-hface2                HICON.
       01 ws-hquestion             HICON.
       01 ws-hradio                HICON.
       01 ws-hcheck                HICON.
       01 ws-module                HMODULE.

       01 ws-sm-cxsmicon           int.
       01 ws-sm-cysmicon           int.
       01 ws-icony                 long.
       01 ws-iconx                 long.

       01  ws-active-window-list.
           03  ws-window-table occurs max-active-windows
                               indexed by ws-active-win.
               05  ws-win-hwnd     HWND.
               05  ws-oldwinproc   procedure-pointer.

      ***********************************************************
      *
      *    This table holds the menu text that is to be matched
      *    and also the corresponding ICON for the menu item.
      ***********************************************************

       01  ws-menu-matching-list.
           03  ws-menu-table occurs max-active-windows
                               indexed by ws-active-win.
               05  ws-menu-text    pic x(max-menu-string).
               05  ws-menu-hicon   hicon.

       01  ws-sub1                 int.

       01  ws-last-hwnd            HWND value null.

       local-storage section.
       01 ls-menu-count            int.
       01 ls-menuitem              uint.
       01 ls-bool                  BOOL.
       01 ls-menuinfo              MENUITEMINFO.
       01 ls-submenu               HMENU.


       linkage section.

       copy "ds-call.cpy".
       COPY "customer.CPB".

      ***** Call Back Parameters

      *01  thelparam            LPARAM.
      *01  child-handle         HWND.

       01 lnk-hwnd                     HWND.
       01 lnk-iMessage                 UINT.
       01 lnk-wParam.
           03 lnk-wParam-LoWord        pic 9(4) comp-5.
           03 lnk-wParam-HiWord        pic 9(4) comp-5.
       01 lnk-lParam.
           03 lnk-lParam-LoWord        pic 9(4) comp-5.
           03 lnk-lParam-HiWord        pic 9(4) comp-5.
       01 lnk-lParam-ptr pointer redefines lnk-lparam.

       01  lnk-sizeinfo             MINMAXINFO.

       01  lnk-anHMENU              HMENU.

       01  lnk-MEASUREITEMSTRUCT     MEASUREITEMSTRUCT.
       01  lnk-DRAWITEMSTRUCT        DRAWITEMSTRUCT.
       01  lnk-MENUITEMDETAIL.
           03  LNK-menu-str            pic x(100).
           03  LNK-menu-str-len        pic 9(9) comp-5 value 0.
           03  LNK-menu-icon           HICON.
           03  LNK-menu-ftype          UINT.


       procedure division using DSC-Control-Block
                                customer-Data-Block.


       main-section section.
      *    goback
           CALL "cob32api"  *> Makes OS DLLs available for INT level code

           if first-time-in     *> Determine First time initialization processing.
               perform init-proc
               set not-first-time-in to true
           end-if



           if customer-subclassflg = 1

      ***** Load some Icons for Menu

               perform load-icons-from-dll

               perform own-draw-menu
               set mywinproc to entry "winproc"
               call winapi "SetWindowLongA" using by value customer-mainhwin
                                                 by value GWL-WNDPROC
                                                 by value mywinproc
                   returning ws-retcode
               end-call

      ***** Store old WinProc
               perform varying ws-sub1 from 1 by 1 until ws-sub1 > max-active-windows
                   if ws-win-hwnd(ws-sub1) = null
                       move customer-mainhwin(1:) to ws-win-hwnd(ws-sub1)(1:)
                       move oldwinproc        to ws-oldwinproc(ws-sub1)
                       exit perform
                   end-if
                   if ws-sub1 = max-active-windows
                       display "ERROR : No active Window Slots Left"
                       stop run
                   end-if
               end-perform


      ***** Set up some Colours

               initialize ws-rgb
               move 190 to ws-rgb-red    *>
               move 190 to ws-rgb-green  *>  GREY
               move 190 to ws-rgb-blue   *>
               move ws-rgb to ws-colour-grey

               perform setup-gradient-colors

           end-if
           if customer-subclassflg = 2
               set mywinproc to entry "winproc"
               call winapi "SetWindowLongA" using by value customer-mainhwin
                                                 by value GWL-WNDPROC
                                                 by value oldwinproc
                   returning ws-retcode
               end-call

      ***** Do Some Clean Up Code

               perform clean-up

           end-if

           goback.

       check-call section.

           if retval = 0 then
               display "ERROR"
               stop run
           end-if
           .


      *************************************************************
      *    Initialization Code - Perform 1st time in only
      *************************************************************
       init-proc section.

           move all x"00" to ws-active-window-list
           .






       own-draw-menu section.


           call winapi "GetMenu" using by value customer-mainhwin
               returning anHMENU
           end-call

           perform varying wx-menu from 0 by 1 until 1 = 2
               call winapi "GetSubMenu" using by value anHMENU
                                              by value wx-menu
                   returning asubhmenu
               end-call
               if asubhmenu = null then
                   exit section
               end-if
               call "SetOwnerDraw" using asubhmenu
           end-perform
           .


       OwnDrawProc section.
       entry "SetOwnerDraw" using lnk-anHMENU

           call winapi "GetMenuItemCount" using by value lnk-anHMENU
               returning ls-menu-count

           perform varying ls-menuitem from 0 by 1 until ls-menuitem = ls-menu-count

               move low-values to ls-menuinfo
               move MIIM-FTYPE to fMask of ls-menuinfo
               move length of ls-menuinfo to cbsize of ls-menuinfo
               call winapi "GetMenuItemInfoA"
                       using by value lnk-anhmenu
                             by value ls-menuitem
                             by value 1 size 4
                             by reference ls-menuinfo
                   returning ls-bool
               end-call
               add MFT-OWNERDRAW to ftype of ls-menuinfo
               add MIIM-DATA     to fMask of ls-menuinfo
               add 1 to menu-itemcount
               set menu-icon(menu-itemcount) to null *> Needs to be initialized
               move ftype of ls-menuinfo to menu-ftype(menu-itemcount)
               call winapi "GetMenuStringA" using by value lnk-anhmenu
                                                 by value ls-menuitem
                                                 by reference menu-str(menu-itemcount)
                                                 by value length of menu-str(menu-itemcount)
                                                 by value MF-BYPOSITION
                   returning menu-str-len(menu-itemcount)
               end-call

      *******************************************************
      * A rather inelegant method of specifing the ICON
      *******************************************************
               evaluate true
                   when menu-itemcount = 1
                       move ws-hface1 to menu-icon(menu-itemcount)
                   when menu-itemcount = 3
                       move ws-hdisk to menu-icon(menu-itemcount)
                   when menu-itemcount = 6
                       move ws-hradio to menu-icon(menu-itemcount)
                   when menu-itemcount = 8
                       move ws-hquestion to menu-icon(menu-itemcount)
                   when menu-itemcount = 10
                       move ws-hface2 to menu-icon(menu-itemcount)
                   when menu-itemcount = 13
                       move ws-hradio to menu-icon(menu-itemcount)
                   when menu-itemcount = 14
                       move ws-hquestion to menu-icon(menu-itemcount)
               end-evaluate
      *******************************************************
      * END OF A rather inelegant method of specifing the ICON
      *******************************************************

         *>      move "Hello World" to menu-str(menu-itemcount)
         *>      move 11            to menu-str-len(menu-itemcount)


               set ws-myptr to address of menu-details(menu-itemcount)
               move ws-myptr(1:) to dwItemData(1:) *> of ls-menuinfo


               call winapi "SetMenuItemInfoA"
                       using by value lnk-anhmenu
                             by value ls-menuitem
                             by value 1 size 4
                             by reference ls-menuinfo
                   returning ls-bool
               end-call

              *>  to do add sub menu handling

               call winapi "GetSubMenu" using by value lnk-anHMENU
                                              by value ls-menuitem
                   returning ls-submenu

               if ls-submenu not = null
                   call "SetOwnerDraw" using ls-submenu
               end-if

           end-perform

           goback
           .












       WinProc section.
       entry "WinProc" winapi  using by value lnk-hwnd
                                     by value lnk-iMessage
                                     by value lnk-wParam
                                     by value lnk-lParam.

            move 0 to App-mResult
      *     display lnk-imessage
            evaluate lnk-iMessage
              when WM-GETMINMAXINFO
               set address of lnk-sizeinfo to lnk-lparam-ptr
               move 250 to x of ptmintracksize of lnk-sizeinfo
               move 250 to y of ptmintracksize of lnk-sizeinfo
               call WinAPI OldWinProc using
                           by value lnk-hwnd
                           by value lnk-iMessage
                           by value lnk-wParam
                           by value lnk-lParam
                           returning App-mResult
                end-call
              when wm-drawitem
      *            DISPLAY "WM-DRAWITEM"
                   if lnk-wParam-LoWord not = 0 and
                      lnk-wParam-HiWord not = 0
                       call WinAPI OldWinProc using
                                   by value lnk-hwnd
                                   by value lnk-iMessage
                                   by value lnk-wParam
                                   by value lnk-lParam
                           returning App-mResult
                       end-call
                       exit section
                   end-if
                   set address of lnk-DRAWITEMSTRUCT to lnk-lparam-ptr
                   move itemData of lnk-DRAWITEMSTRUCT to ws-uintptr
                   SET ADDRESS OF lnk-MENUITEMDETAIL to ws-myptr

                   perform drawmenuitem


              when WM-MEASUREITEM
                   set address of lnk-MEASUREITEMSTRUCT to lnk-lparam-ptr
      *            DISPLAY "WM-MEASUREITEM - " itemID of lnk-MEASUREITEMSTRUCT
                   if CtlType of lnk-MEASUREITEMSTRUCT not = ODT-MENU
                       call WinAPI OldWinProc using
                                   by value lnk-hwnd
                                   by value lnk-iMessage
                                   by value lnk-wParam
                                   by value lnk-lParam
                           returning App-mResult
                       end-call
                       exit section
                   end-if
                   move itemData of lnk-MEASUREITEMSTRUCT to ws-uintptr
                   SET ADDRESS OF lnk-MENUITEMDETAIL to ws-myptr


      ***** Check to see if this is a separator bar
                   move MFT-SEPARATOR to ws-result
                   CALL "CBL_AND" USING LNK-menu-ftype
                                        ws-result
                                        by value 4


      ***** Work out the length required for the Text

      ***** Get a DC
                   call winapi "GetDC" using by value lnk-hwnd returning ws-hdc
      ***** Get the Default System Font
                   call winapi "GetStockObject" using by value SYSTEM-FONT
                       returning ws-font
                   end-call
                   call winapi "SelectObject" using by value ws-font
                       returning ws-old-font
                   call winapi "GetTextExtentPoint32A" using by value ws-hdc
                                                             by reference LNK-menu-str
                                                             by value LNK-menu-str-len
                                                             by reference ws-size
                   end-call
                   call winapi "SelectObject" using by value ws-old-font
                       returning ws-font
                   end-call
                   call winapi "DeleteObject" using by value ws-font
                   call winapi "ReleaseDC" using by value ws-hdc

        *>           display cx of ws-size " - " LNK-menu-str-len " - " LNK-menu-str

                   if WS-RESULT > 1
                       move 3 to itemheight of lnk-MEASUREITEMSTRUCT
      *                MOVE 100 to itemwidth of lnk-MEASUREITEMSTRUCT
                   else
                       move 20 to itemheight of lnk-MEASUREITEMSTRUCT
      *                MOVE 100 to itemwidth of lnk-MEASUREITEMSTRUCT
                   end-if
                   compute itemwidth of lnk-MEASUREITEMSTRUCT = (cx of ws-size * .77) + 30

      *       when WM-SYSCOLORCHANGE
      *         DISPLAY "Hello"
              when other
      ****************************************************************
      *     All other messages are despatched to the default         *
      *     window procedure according to the Windows rules          *
      ****************************************************************
               if ws-last-hwnd not = lnk-hwnd  *> Determine if we need to swap OldWinProc
                   perform varying ws-sub1 from 1 by 1 until ws-sub1 > max-active-windows
                       if lnk-hwnd = ws-win-hwnd(ws-sub1)
                           move ws-oldwinproc(ws-sub1) to OldWinProc
                           exit perform
                       end-if
                       if ws-sub1 = max-active-windows
                           display "Error : No Matching Oldwinproc"
                           stop run
                       end-if
                   end-perform
               end-if
               call WinAPI OldWinProc using
                           by value lnk-hwnd
                           by value lnk-iMessage
                           by value lnk-wParam
                           by value lnk-lParam
                           returning App-mResult
                end-call
            end-evaluate

           exit program returning App-mResult.
           .


      *************************************************************
      *
      *************************************************************
       DrawMenuItem section.


      *    display "State=" itemAction of lnk-DRAWITEMSTRUCT
      *            " , ItemState = " itemState of lnk-DRAWITEMSTRUCT

      ***** Perform the checks to determine what state needs to be drawn.

           move itemAction of lnk-DRAWITEMSTRUCT to ws-bittest
           if FUNCTION MOD( ws-bittest , 2) > 0
               SET FLAG-ODA-DRAWENTIRE  TO TRUE
           end-if
           divide ws-bittest by 2 giving ws-bittest
           if FUNCTION MOD( ws-bittest , 2) > 0
               SET FLAG-ODA-SELECT  TO TRUE
           end-if
           divide ws-bittest by 2 giving ws-bittest
           if FUNCTION MOD( ws-bittest , 2) > 0
               set FLAG-ODA-FOCUS  TO TRUE
           end-if

      ***** Clear the base menu area. This will be white.

           move 255 to ws-rgb-red
           move 255 to ws-rgb-green
           move 255 to ws-rgb-blue

           call winapi "CreatePen" using by value ps-solid
                                         by value 0
                                         by value ws-rgb
               returning ws-base-pen
           end-call

      ***** Set the Background Color of the DC. This is so the Text has the correct
      ***** background color when its output.

           call winapi "SetBkColor" using by value 1hdc of lnk-DRAWITEMSTRUCT
                                          by value ws-rgb
               returning ws-rgb
           end-call


           call winapi "SelectObject" using by value 1hdc of lnk-DRAWITEMSTRUCT
                                            by value ws-base-pen
               returning ws-old-pen
           end-call


           call winapi "Rectangle" using by value 1hdc of lnk-DRAWITEMSTRUCT
                                         by value 1left of rcitem
                                         by value 1top of rcitem
                                         by value 1right of rcitem
                                         by value 1bottom of rcitem
               returning ls-bool
           call winapi "SelectObject" using by value 1hdc of lnk-DRAWITEMSTRUCT
                                            by value ws-old-pen
               returning ws-base-pen
           end-call
           call winapi "DeleteObject" using by value ws-base-pen
               returning ls-bool
           end-call

      ***** Check to see if this menu item has been greyed out
      ***** ie disabled.

           move ODS-GRAYED to ws-result
           CALL "CBL_AND" USING itemstate of lnk-DRAWITEMSTRUCT
                                ws-result
                                by value 4
           if ws-result > 1 then
               SET FLAG-DISABLE TO TRUE
      *        display "disabled"
           else
               SET FLAG-ENABLED TO TRUE
      *        display "Enabled"
           end-if

           move ODS-SELECTED to ws-result
           CALL "CBL_AND" USING itemstate of lnk-DRAWITEMSTRUCT
                                ws-result
                                by value 4

      ***** Decide here if this item is SELECTED and draw a
      ***** Colored Bar if it is.
      ***** On a menu this bar would be Blue.
      ***** We will use yellow as the base but also draw a
      ***** Black Rectangle around the select area.

           if (FLAG-ODA-SELECT and ws-result = 1) or
              (FLAG-ODA-DRAWENTIRE and ws-result = 1) then

               *> Create a BLACK Pen for teh rectangle

               move 0 to ws-rgb-red
               move 0 to ws-rgb-green
               move 0 to ws-rgb-blue

               call winapi "CreatePen" using by value ps-solid
                                             by value 0
                                             by value ws-rgb
                  returning ws-base-pen
               end-call

               call winapi "SelectObject" using by value 1hdc of lnk-DRAWITEMSTRUCT
                                                by value ws-base-pen
                   returning ws-old-pen
               end-call

               move 255 to ws-rgb-red    *>
               move 255 to ws-rgb-green  *>  A "Yellowy" colour
               move 198 to ws-rgb-blue   *>


               call winapi "CreateSolidBrush" using by value ws-rgb
                  returning ws-base-brush
               end-call

               call winapi "SelectObject" using by value 1hdc of lnk-DRAWITEMSTRUCT
                                                by value ws-base-brush
                   returning ws-old-brush
               end-call
               call winapi "Rectangle" using by value 1hdc of lnk-DRAWITEMSTRUCT
                                             by value 1left of rcitem
                                             by value 1top of rcitem
                                             by value 1right of rcitem
                                             by value 1bottom of rcitem
                   returning ls-bool
               call winapi "SelectObject" using by value 1hdc of lnk-DRAWITEMSTRUCT
                                             by value ws-old-brush
                   returning ws-base-brush
               end-call
               call winapi "DeleteObject" using by value ws-base-brush
                   returning ls-bool
               end-call

               call winapi "SelectObject" using by value 1hdc of lnk-DRAWITEMSTRUCT
                                             by value ws-old-pen
                   returning ws-base-pen
               end-call
               call winapi "DeleteObject" using by value ws-base-pen
                   returning ls-bool
               end-call

      ***** Set the Background Color of the DC. This is so the Text has the correct
      ***** background color when its output.

               call winapi "SetBkColor" using by value 1hdc of lnk-DRAWITEMSTRUCT
                                              by value ws-rgb
                   returning ws-rgb
               end-call
           else
               perform draw-gradient-at-side    *> Gradient is not drawn for a selected Item
           end-if

      *    if FLAG-ODA-DRAWENTIRE
      *        perform draw-gradient-at-side
      *    end-if


      ***** Check to see if this is a separator bar

           move MFT-SEPARATOR to ws-result
           CALL "CBL_AND" USING LNK-menu-ftype
                                ws-result
                                by value 4
           if WS-RESULT > 1
      ***** Draw line in the middle in colour grey
               call winapi "CreatePen" using by value ps-solid
                                             by value 0
                                             by value ws-colour-grey
                   returning ws-base-pen
               end-call

               call winapi "SelectObject" using by value 1hdc of lnk-DRAWITEMSTRUCT
                                                by value ws-base-pen
                   returning ws-old-pen
               end-call
               compute ws-y = 1bottom of rcitem + ((1top of rcitem - 1bottom of rcitem)/2)
               compute ws-x = 1left of rcitem + 30
               call winapi "MoveToEx" using by value 1hdc of lnk-DRAWITEMSTRUCT
                                            by value ws-x
                                            by value ws-y
                                            by value 0
               end-call
               call winapi "LineTo" using by value 1hdc of lnk-DRAWITEMSTRUCT
                                          by value 1right of rcitem
                                          by value ws-y
               end-call
               call winapi "SelectObject" using by value 1hdc of lnk-DRAWITEMSTRUCT
                                                by value ws-old-pen
                   returning ws-base-pen
               end-call
               call winapi "DeleteObject" using by value ws-base-pen
                   returning ls-bool
               end-call
           else
               if flag-disable
      ***** Grey the text for a disabled Menu Item
                   call winapi "SetTextColor" using by value 1hdc of lnk-DRAWITEMSTRUCT
                                                    by value ws-colour-grey
                       returning ws-text-col
                   end-call
               end-if
               move rcitem to ws-rect
               add 30 to 1left of ws-rect

               compute ws-format = DT-LEFT + DT-SINGLELINE + DT-VCENTER + DT-EXPANDTABS
               move LNK-menu-str to ws-str
               move x"00"        to ws-str(LNK-menu-str-len + 1:1)
               call winapi "DrawTextA" using by value 1hdc of lnk-DRAWITEMSTRUCT
                                             by reference ws-str
                                             by value LNK-menu-str-len
                                             by reference ws-rect
                                             by value ws-format
               end-call
               if flag-disable
                   call winapi "SetTextColor" using by value 1hdc of lnk-DRAWITEMSTRUCT
                                                    by value ws-text-col
                       returning ws-colour-grey
                   end-call
               end-if
           end-if

           move ODS-CHECKED to ws-result
           CALL "CBL_AND" USING itemstate of lnk-DRAWITEMSTRUCT
                                ws-result
                                by value 4
           if WS-RESULT > 1
               perform draw-check-mark
               exit section      *> SKIP ICON Drawing if checked. Can't Display Both
           end-if




      ***** Now Draw the ICON (Only if one is actually required)

           if LNK-menu-icon not = null

               compute ws-icony = 1top of rcitem + (((1bottom of rcitem  - 1top of rcitem) -  ws-sm-cysmicon ) / 2)
               compute ws-iconx = 1left of rcitem + ((30 -  ws-sm-cysmicon ) / 2)

      *        display "icon" lnk-menu-icon
      *        add DI-NORMAL DI-DEFAULTSIZE giving ws-sm-cxsmicon
      *        move DI-NORMAL to ws-sm-cxsmicon
               call winapi "DrawIconEx" using by value 1hdc of lnk-DRAWITEMSTRUCT
                                            by value icon-offset-x
                                            by value ws-icony
                                            by value lnk-menu-icon
      *                                     by value ws-sm-cxsmicon
      *                                     by value ws-sm-cysmicon
                                            by value 0
                                            by value 0
                                            by value 0
                                            by value 0
      *                                     by value ws-sm-cxsmicon
                                            by value DI-NORMAL
                   returning retval
               end-call

           end-if


           .


      *************************************************************
      *  This section will graw the gradient bar at the side of
      *  the menu. This requires the setting up of a mesh array
      *  structure. In this case its just a gradual lightening
      *  of the colour hotizontally.
      *************************************************************
       draw-gradient-at-side section.

           initialize ws-mesharray
           move 0 to UpperLeft of ws-mesh(1)
           move 1 to LowerRight of ws-mesh(1)

           move 2 to dwNumVertex
           move 1 to dwNumMesh

           move GRADIENT-FILL-RECT-H to dwMode
      ***** Fill Vertically so it looks more like Outlook 2003
      *    move GRADIENT-FILL-RECT-V to dwMode

           initialize ws-TRIVERTEXArray
           move 3 to x of ws-TRIVERTEX(1)
           move 0 to x of ws-TRIVERTEX(1)
           move 1top of rcitem to y of ws-TRIVERTEX(1)
      *    move h"ff00"  to red of ws-TRIVERTEX(1)

      ****** INFO : multiplying by h"100" (ie 2 ** 8) just makes it the
      ****** Most Significant Byte
           move ws-grad-toRGB to ws-rgb
           compute red of ws-TRIVERTEX(1) = ws-rgb-red * h"100"
           compute green of ws-TRIVERTEX(1) = ws-rgb-green * h"100"
           compute blue of ws-TRIVERTEX(1) = ws-rgb-blue * h"100"

           move 1right of rcitem  to x of ws-TRIVERTEX(2)
           move 1bottom of rcitem to y of ws-TRIVERTEX(2)
           add x of ws-TRIVERTEX(1) 23 giving x of ws-TRIVERTEX(2)


           move ws-grad-fromRGB to ws-rgb
           compute red of ws-TRIVERTEX(2) = ws-rgb-red * h"100"
           compute green of ws-TRIVERTEX(2) = ws-rgb-green * h"100"
           compute blue of ws-TRIVERTEX(2) = ws-rgb-blue * h"100"

      *    display "left=" 1left of rcitem
      *            " , top= "1top of rcitem
      *            " , right=" 1right of rcitem
      *            " , bottom = " 1bottom of rcitem " , " 1hdc of lnk-DRAWITEMSTRUCT
           set ws-proc to entry "Msimg32.dll"
           call winapi "GradientFill"
              using by value     1hdc of lnk-DRAWITEMSTRUCT
                    by reference ws-TRIVERTEXArray
                    by value     dwNumVertex
                    by reference ws-mesharray
                    by value     dwNumMesh
                    by value     dwMode
              returning ws-retval
           end-call
           .


      **************************************************************
      *  Setup the Colours to be used in the gradient drawing
      **************************************************************
       setup-gradient-colors section.

      ***** We will use the Inactive caption color as the basis of the
      ***** gradiant. This will then vary according to the system
      ***** theme and should look OK on most systems.

           call winapi "GetSysColor" using by value COLOR-INACTIVECAPTION
               returning ws-grad-fromRGB
           end-call
           move ws-grad-fromRGB to ws-rgb

      ***** Set up Linkage to Conversion Routines
      ***** This converts to HSL so that we can make the
      ***** colors lighter.
      ***** You cannot make RGB based values lighter.

           move ws-rgb-red   to ws-rgb-r
           move ws-rgb-green to ws-rgb-g
           move ws-rgb-blue  to ws-rgb-b
           call "rgbtohsl" using ws-RGB-R
                                 ws-RGB-G
                                 ws-RGB-B
                                 ws-HSL-H
                                 ws-HSL-S
                                 ws-HSL-L

      ***** make 20% lighter

           compute ws-HSL-L = ws-hsl-l + ( ws-hsl-l / 5)
           if ws-HSL-L > 1   *> White - One is the limit so make sure
               move 1 to ws-hsl-l   *> we don't go above it.
           end-if

      ***** Now Convert Back to RGB so we can use this in Windows
      ***** Routines
           move 0 to ws-rgb-r
           move 0 to ws-rgb-g
           move 0 to ws-rgb-b
           call "hsltorgb" using ws-RGB-R
                                 ws-RGB-G
                                 ws-RGB-B
                                 ws-HSL-H
                                 ws-HSL-S
                                 ws-HSL-L

      ***** Store the returned Colour

           move ws-rgb-r to ws-rgb-red
           move ws-rgb-g to ws-rgb-green
           move ws-rgb-b to ws-rgb-blue

           move ws-rgb  to ws-grad-toRGB
           .

      *************************************************
      *    Draw the checkmark if applicable
      *************************************************
       draw-check-mark section.


               *> Create a BLACK Pen for teh rectangle

               move 0 to ws-rgb-red
               move 0 to ws-rgb-green
               move 0 to ws-rgb-blue

               call winapi "CreatePen" using by value ps-solid
                                             by value 0
                                             by value ws-rgb
                  returning ws-base-pen
               end-call

               call winapi "SelectObject" using by value 1hdc of lnk-DRAWITEMSTRUCT
                                                by value ws-base-pen
                   returning ws-old-pen
               end-call

               move 239 to ws-rgb-red    *>
               move 213 to ws-rgb-green  *>  A "Yellowy" colour
               move 63  to ws-rgb-blue   *>


               call winapi "CreateSolidBrush" using by value ws-rgb
                  returning ws-base-brush
               end-call

               call winapi "SelectObject" using by value 1hdc of lnk-DRAWITEMSTRUCT
                                                by value ws-base-brush
                   returning ws-old-brush
               end-call

      *    compute ws-icony = 1top of rcitem + (((1bottom of rcitem  - 1top of rcitem) -  ws-sm-cysmicon ) / 2)
      *    compute ws-iconx = 1left of rcitem + ((30 -  ws-sm-cysmicon ) / 2)

           compute 1left of ws-rect = 1left of rcitem + icon-offset-x - 1
           compute 1top  of ws-rect = 1top of rcitem + (((1bottom of rcitem  - 1top of rcitem) -  ws-sm-cysmicon ) / 2) - 1
           compute 1right of ws-rect = 1left of ws-rect + ws-sm-cysmicon + 2
           compute 1bottom of ws-rect = 1top of ws-rect + ws-sm-cysmicon + 2

               call winapi "Rectangle" using by value 1hdc of lnk-DRAWITEMSTRUCT
                                             by value 1left of ws-rect
                                             by value 1top of ws-rect
                                             by value 1right of ws-rect
                                             by value 1bottom of ws-rect
      *                                      by value 1left of rcitem
      *                                      by value 1top of rcitem
      *                                      by value 1right of rcitem
      *                                      by value 1bottom of rcitem
                   returning ls-bool
               call winapi "SelectObject" using by value 1hdc of lnk-DRAWITEMSTRUCT
                                             by value ws-old-brush
                   returning ws-base-brush
               end-call
               call winapi "DeleteObject" using by value ws-base-brush
                   returning ls-bool
               end-call

               call winapi "SelectObject" using by value 1hdc of lnk-DRAWITEMSTRUCT
                                             by value ws-old-pen
                   returning ws-base-pen
               end-call
               call winapi "DeleteObject" using by value ws-base-pen
                   returning ls-bool
               end-call

      ***** Set the Background Color of the DC. This is so the Text has the correct
      ***** background color when its output.

      *        call winapi "SetBkColor" using by value 1hdc of lnk-DRAWITEMSTRUCT
      *                                       by value ws-rgb
      *            returning ws-rgb
      *        end-call






















           compute ws-icony = 1top of rcitem + (((1bottom of rcitem  - 1top of rcitem) -  ws-sm-cysmicon ) / 2)
           compute ws-iconx = 1left of rcitem + ((30 -  ws-sm-cysmicon ) / 2)
           call winapi "DrawIconEx" using by value 1hdc of lnk-DRAWITEMSTRUCT
                                        by value icon-offset-x
                                        by value ws-icony
                                        by value ws-hcheck
                                        by value 0
                                        by value 0
                                        by value 0
                                        by value 0
                                        by value DI-NORMAL
               returning retval
           end-call


           .




      *************************************************
      *    Load ICONS from a DLL.
      *************************************************
       load-icons-from-dll section.


      ***** Request the small ICON Size from the system. This is
      ***** Is the ICON size on the top left of a window on the System
      ***** menu button.

          call winapi "GetSystemMetrics" using by value sm-cxsmicon
               returning ws-sm-cxsmicon
          call winapi "GetSystemMetrics" using by value sm-cysmicon
               returning ws-sm-cysmicon

          INVOKE module "newZ" USING z"cust.dll"
                          RETURNING ws-Resource

          invoke ws-resource "getid" returning ws-module

      ***** Use LoadImage so that we get the small ICON Size
      ***** Other api routines will just return the large
      ***** ICON Size.

          call winapi "LoadImageA" using by value ws-module
                                         by value IDI-DISK1
                                         by value IMAGE-ICON
                                         by value ws-sm-cxsmicon
                                         by value ws-sm-cysmicon
                                         by value LR-DEFAULTSIZE
               returning ws-hdisk
          end-call
          call winapi "LoadImageA" using by value ws-module
                                         by value IDI-FACE1
                                         by value IMAGE-ICON
                                         by value ws-sm-cxsmicon
                                         by value ws-sm-cysmicon
                                         by value LR-DEFAULTSIZE
               returning ws-hface1
          end-call
          call winapi "LoadImageA" using by value ws-module
                                         by value IDI-FACE2
                                         by value IMAGE-ICON
                                         by value ws-sm-cxsmicon
                                         by value ws-sm-cysmicon
                                         by value LR-DEFAULTSIZE
               returning ws-hface2
          end-call
          call winapi "LoadImageA" using by value ws-module
                                         by value IDI-QUEST
                                         by value IMAGE-ICON
                                         by value ws-sm-cxsmicon
                                         by value ws-sm-cysmicon
                                         by value LR-DEFAULTSIZE
               returning ws-hquestion
          end-call
          call winapi "LoadImageA" using by value ws-module
                                         by value IDI-RADIO
                                         by value IMAGE-ICON
                                         by value ws-sm-cxsmicon
                                         by value ws-sm-cysmicon
                                         by value LR-DEFAULTSIZE
               returning ws-hradio
          end-call
          call winapi "LoadImageA" using by value ws-module
                                         by value IDI-CHECK
                                         by value IMAGE-ICON
                                         by value ws-sm-cxsmicon
                                         by value ws-sm-cysmicon
                                         by value LR-DEFAULTSIZE
               returning ws-hcheck
          end-call

      ***** Class Library just loaded the large ICONS so use the method
      ***** Above.
      *   move IDI-DISK1 to ws-ResourceID
      *   INVOKE iconData "fromResource" USING ws-Resource
      *                                        ws-ResourceID
      *                              RETURNING ws-disk
      *   invoke ws-disk "getid" returning ws-hdisk

      *   move IDI-FACE1 to ws-ResourceID
      *   INVOKE iconData "fromResource" USING ws-Resource
      *                                        ws-ResourceID
      *                              RETURNING ws-face1
      *   invoke ws-face1 "getid" returning ws-hface1
      *
      *   move IDI-FACE2 to ws-ResourceID
      *   INVOKE iconData "fromResource" USING ws-Resource
      *                                        ws-ResourceID
      *                              RETURNING ws-face2
      *   invoke ws-face2 "getid" returning ws-hface2
      *
      *   move IDI-QUEST to ws-ResourceID
      *   INVOKE iconData "fromResource" USING ws-Resource
      *                                        ws-ResourceID
      *                              RETURNING ws-question
      *   invoke ws-question "getid" returning ws-hquestion
      *
      *   move IDI-RADIO to ws-ResourceID
      *   INVOKE iconData "fromResource" USING ws-Resource
      *                                        ws-ResourceID
      *                              RETURNING ws-radio
      *   invoke ws-radio "getid" returning ws-hradio
          .


      *************************************************
      *    Try and prevent Memory Leaks
      *************************************************
       clean-up section.


           invoke ws-Resource "finalize" returning ws-resource
      *    invoke ws-disk     "finalize" returning ws-disk
      *    invoke ws-face1    "finalize" returning ws-face1
      *    invoke ws-face2    "finalize" returning ws-face2
      *    invoke ws-question "finalize" returning ws-question
      *    invoke ws-radio    "finalize" returning ws-radio
           .
