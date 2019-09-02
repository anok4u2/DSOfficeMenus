      $set sourceformat(variable)
      ********************************************************************
      *
      *    Author - David Sands - Micro Focus EMEA Technical Support
      *
      *    This routine is a COBOL Implementation of an Algorithm that
      *    converts a colour expressed as Hue , Saturation , Lightness (HSL).
      *    to a colour expressed as Red , Green and Blue components (RGB)
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

       01  ws-var1             pic s9(3)v9(15) comp-5.
       01  ws-var2             pic s9(3)v9(15) comp-5.
       01  ws-temp1            pic s9(3)v9(15) comp-5.
       01  ws-temp2            pic s9(3)v9(15) comp-5.

       01  ws-rgb              pic s9(12)v9(6) comp-5.

       local-storage section.


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
                      ws-var1
                      ws-var2
                      ws-temp1
                      ws-temp2
                      ws-rgb

           if lnk-hsl-s = 0
               compute lnk-rgb-r rounded = lnk-hsl-l * 255
               compute lnk-rgb-g rounded = lnk-hsl-l * 255
               compute lnk-rgb-b rounded = lnk-hsl-l * 255
           else
               if lnk-hsl-l < 0.5
                   compute ws-var2 = lnk-hsl-L * ( 1 + lnk-hsl-S )
               else
                   compute ws-var2 = ( lnk-hsl-L + lnk-hsl-S ) - ( lnk-hsl-S * lnk-hsl-L )
               end-if
               compute ws-var1 = 2 * lnk-hsl-L - ws-var2

               compute ws-temp1 = lnk-hsl-h + (1 / 3)
               perform Hue-2-rgb
               compute lnk-rgb-r rounded = ws-rgb * 255

               move lnk-hsl-h to ws-temp1
               perform Hue-2-rgb
               compute lnk-rgb-g rounded = ws-rgb * 255

               compute ws-temp1 = lnk-hsl-h - (1 / 3)
               perform Hue-2-rgb
               compute lnk-rgb-b rounded = ws-rgb * 255
           end-if
           exit program
           .

       Hue-2-RGB section.

           if ws-temp1 < 0
               compute ws-temp1 = ws-temp1 + 1
           end-if
           if ws-temp1 > 1
               compute ws-temp1 = ws-temp1 - 1
           end-if
           if ((6 * ws-temp1) < 1)
               compute ws-rgb = ( ws-var1 + ( ws-var2 - ws-var1 ) * 6 * ws-temp1 )
               exit section
           end-if
           if ((2 * ws-temp1) < 1)
               move ws-var2 to ws-rgb
               exit section
           end-if
           if ((3 * ws-temp1) < 2)
               compute ws-rgb = ( ws-var1 + ( ws-var2 - ws-var1 ) * ( ( 2 / 3 ) - ws-temp1 ) * 6 )
               exit section
           end-if
           move ws-var1 to ws-rgb
           .
