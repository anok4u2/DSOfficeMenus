      $set sourceformat(variable)
      ********************************************************************
      *
      *    Author - David Sands - Micro Focus EMEA Technical Support
      *
      *    This routine is a COBOL Implementation of an Algorithm that
      *    converts a colour expressed as Red , Green and Blue components (RGB)
      *    to a colour expressed as Hue , Saturation , Lightness (HSL).
      *
      *    Colours in Microsoft Windows are express in terms of RGB.
      *
      *    This conversion is required in order to make colours lighter or
      *    darker. If you just subtract values from the RGB elements this just
      *    changes the colour rather then darken it.
      *
      *    However when a colour is expressed as a HSL value then the L part
      *    of this can be amended to lighten or darker a colour.
      *
      *    The HSL would need conversion back to RGB for use by Windows.
      *
      ********************************************************************
       working-storage section.
       01  ws-RGB-R            pic s9v9(17)  comp-5.
       01  ws-RGB-G            pic s9v9(17)  comp-5.
       01  ws-RGB-B            pic s9v9(17)  comp-5.
       01  ws-RGB-min          pic s9v9(17)  comp-5.
       01  ws-RGB-max          pic s9v9(17)  comp-5.
       01  ws-RGB-del          pic s9v9(17)  comp-5.

       01  ws-del-r            pic s9v9(17)  comp-5.
       01  ws-del-g            pic s9v9(17)  comp-5.
       01  ws-del-b            pic s9v9(17)  comp-5.

       01  ws-H                pic s9v9(17)  comp-5.
       01  ws-S                pic s9v9(17)  comp-5.
       01  ws-L                pic s9v9(17)  comp-5.

       linkage section.
       copy "rgbhsl.cpy" replacing ==:TAG:== by ==lnk-==.

       procedure division using lnk-RGB-R
                                lnk-RGB-G
                                lnk-RGB-B
                                lnk-HSL-H
                                lnk-HSL-S
                                lnk-HSL-L.

       startit section.

           initialize ws-RGB-R
                      ws-RGB-G
                      ws-RGB-B
                      ws-RGB-min
                      ws-RGB-max
                      ws-RGB-del
                      ws-del-r
                      ws-del-g
                      ws-del-b
                      ws-H
                      ws-S
                      ws-L

           compute ws-rgb-r = lnk-RGB-R / 255
           compute ws-rgb-g = lnk-RGB-G / 255
           compute ws-rgb-b = lnk-RGB-B / 255

           move function min(ws-rgb-r , ws-rgb-g , ws-rgb-b) to ws-rgb-min
           move function max(ws-rgb-r , ws-rgb-g , ws-rgb-b) to ws-rgb-max
           compute ws-rgb-del = ws-rgb-max - ws-rgb-min

           compute ws-L = (ws-rgb-max + ws-rgb-min) / 2

           if ws-rgb-del = 0
               move 0 to ws-H
               move 0 to ws-S
           else
               if ws-L < 0.5
                   compute ws-S = ws-rgb-del / (ws-rgb-max - ws-rgb-min)
               else
                   compute ws-S = ws-rgb-del / (2 - ws-rgb-max - ws-rgb-min)
               end-if
               compute ws-del-r = ((( ws-rgb-max - ws-rgb-R ) / 6 ) + ( ws-rgb-del / 2 ) ) / ws-rgb-del
               compute ws-del-g = ((( ws-rgb-max - ws-rgb-g ) / 6 ) + ( ws-rgb-del / 2 ) ) / ws-rgb-del
               compute ws-del-b = ((( ws-rgb-max - ws-rgb-b ) / 6 ) + ( ws-rgb-del / 2 ) ) / ws-rgb-del
               if ws-rgb-r = ws-rgb-max
                   compute ws-H = ws-del-B - ws-del-G
               else
                   if ws-rgb-g = ws-rgb-max
                       compute ws-H = ( 1 / 3 ) + ws-del-R - ws-del-B
                   else
                       if ws-rgb-b = ws-rgb-max
                           compute ws-H = ( 2 / 3 ) + ws-del-G - ws-del-R
                       end-if
                   end-if
               end-if
               if ws-h < 0
                   add 1 to ws-h
               end-if
               if ws-h > 1
                   subtract 1 from ws-h
               end-if
           end-if

           move ws-h   to lnk-hsl-h
           move ws-s   to lnk-hsl-s
           move ws-l   to lnk-hsl-l

           exit program
