       identification division.
       program-id. brickbreak.
       environment division.

       data division.
       working-storage section.
       01 w    usage binary-long value 640.       *> window width
       01 h    usage binary-long value 480.       *> window height
       01 n    pic X(11).                         *> window name
       01 bg   usage binary-long value -15198184. *> window background color
       01 clo  usage binary-char.                 *> should the window close

       01 spc  usage binary-long value 32.  *> space key code
       01 rgt  usage binary-long value 262. *> right key code
       01 lft  usage binary-long value 263. *> left key code
       01 isp  usage binary-char.           *> is key pressed

       01 px   usage binary-long.                *> paddle x
       01 py   usage binary-long.                *> paddle y
       01 pw   usage binary-long value 120.      *> paddle width
       01 phe  usage binary-long value 20.       *> paddle height
       01 pdx  usage binary-long value 0.        *> paddle x direction
       01 pco  usage binary-long value -5592406. *> paddle color
       01 ps   usage binary-long value 5.        *> paddle speed

       *> if the ball (square) speed is greater than the paddle speed
       *> there is a chance the ball (square) will get stuck inside the paddle
       78 ixs value 5.  *> initial ball (square) x speed
       78 iys value -5. *> initial ball (square) x speed

       01 sx   usage binary-long.                 *> ball (square) x position
       01 sy   usage binary-long.                 *> ball (square) y position
       01 ss   usage binary-long value 18.        *> ball (square) size
       01 sc   usage binary-long value -13378049. *> ball (square) color
       01 sf   usage binary-long value 1.         *> should the ball follow the paddle
       01 sxs  usage binary-long value ixs.       *> ball (square) x speed
       01 sys  usage binary-long value iys.       *> ball (square) y speed

       01 fps  usage binary-long value 60. *> frames per second

       01 bw   usage binary-long value 50.         *> brick width
       01 bh   usage binary-long value 22.         *> brick height
       01 bc   usage binary-long value -197639681. *> brick color
       01 bgap usage binary-long value 15.         *> gap between bricks
       01 vgap usage binary-long value 20.         *> vertical gap between bricks and window
       01 hgap usage binary-long.                  *> horizontal gap between bricks and window

       78 dw value 8. *> amount of brick columns
       78 dh value 5. *> amount of brick rows

       01 bricks.
        05 bricks-col occurs dw times.
         10 bricks-row occurs dh times.
          15 brick-x usage binary-long. *> brick x position
          15 brick-y usage binary-long. *> brick y position
          15 brick-v usage binary-long. *> if the brick is visible

       01 i usage binary-long. *> used in for loops
       01 j usage binary-long. *> used in for loops

       78 cn value 0. *> Collision none
       78 ct value 1. *> Collision top
       78 cb value 2. *> Collision bottom
       78 cl value 3. *> Collision left
       78 cr value 4. *> Collision right

       *> this rectangle will be used in collision checking
       *> set this before calling 'check-colision' function (section)
       01 tx usage binary-long.
       01 ty usage binary-long.
       01 tw usage binary-long.
       01 th usage binary-long.
       *> this will have a different value depending on the type of collision
       *> check the constants defined above to check what each value means
       01 cc usage binary-long value cn.

       procedure division.
        move Z"BrickBreak" to n.
        call "InitWindow" using by value w by value h by reference n.
        call "SetTargetFPS" using by value fps.

        compute px = w / 2 - pw / 2.
        compute py = h - phe - 15.

        compute hgap = (w - (dw * bw + dw * bgap)) / 2.
        perform varying i from 1 by 1 until i > dw
         perform varying j from 1 by 1 until j > dh
          compute brick-x(i, j) = (i - 1) * bw + (i - 1) * bgap + hgap
          compute brick-y(i, j) = (j - 1) * bh + (j - 1) * bgap + vgap
          compute brick-v(i, j) = 1
         end-perform
        end-perform.

        call "WindowShouldClose" returning clo
        perform until clo is equal to 1
         call "BeginDrawing"
         call "ClearBackground" using by value bg

         move 0 to pdx
         call "IsKeyDown" using by value lft returning isp
         if isp is equal to 1 then
          compute pdx = -1
         end-if
         call "IsKeyDown" using by value rgt returning isp
         if isp is equal to 1 then
          compute pdx = 1
         end-if

         compute px = px + (pdx * ps)
         if px is less than 0 then
             move 0 to px
         end-if
         if px + pw is greater than w then
             compute px = w - pw
         end-if

         if sf is equal to 1 then
          compute sx = px + (pw / 2) - (ss / 2)
          compute sy = py - ss
         else
          *> bounce ball (square) when it hits the sides of the window
          if sx + sxs is less than 0
             or sx + ss + sxs is greater than w then
           compute sxs = sxs * -1
          end-if
          *> bounce ball (square) when it hits the top of the window
          if sy + sys is less than 0 then
           compute sys = sys * -1
          end-if

          move px  to tx
          move py  to ty
          move pw  to tw
          move phe to th
          perform check-colision

          *> TODO: handle colision with the top of the bar in a better way
          if cc is equal to ct then
           if pdx is not equal to 0 then
            compute sxs = ixs * pdx
           end-if
           compute sys = sys * -1
          end-if

          if (cc is equal to cl) or (cc is equal to cr) then
           compute sxs = sxs * -1
          end-if

          *> check collision between ball (square) and bricks
          perform varying i from 1 by 1 until i > dw
           perform varying j from 1 by 1 until j > dh
            if brick-v(i, j) is equal to 1 then
             move brick-x(i, j) to tx
             move brick-y(i, j) to ty
             move bw to tw
             move bh to th
             perform check-colision
             if cc is not equal to cn then
              move 0 to brick-v(i, j)
             end-if
             if (cc is equal to ct) or (cc is equal to cb) then
              compute sys = sys * -1
             end-if
             if (cc is equal to cl) or (cc is equal to cr) then
              compute sxs = sxs * -1
             end-if
            end-if
           end-perform
          end-perform

          *> reset ball (square) if it goes offscreen
          if sy + sys is greater than h + (ss * 3) then
           move ixs to sxs
           move iys to sys
           move 1 to sf
          end-if

          compute sx = sx + sxs
          compute sy = sy + sys
         end-if

         call "IsKeyPressed" using by value spc returning isp
         if sf is equal to 1 and isp is equal to 1 then
          move 0 to sf
         end-if

         call "DrawRectangle" using
          by value px by value py
          by value pw by value phe
          by value pco

         perform varying i from 1 by 1 until i > dw
          perform varying j from 1 by 1 until j > dh
           if brick-v(i, j) is equal to 1 then
            call "DrawRectangle" using
             by value brick-x(i, j) by value brick-y(i, j)
             by value bw by value bh
             by value bc
           end-if
          end-perform
         end-perform

         call "DrawRectangle" using
          by value sx by value sy
          by value ss by value ss
          by value sc

         call "EndDrawing"
         call "WindowShouldClose" returning clo
        end-perform.

        call "CloseWindow".
        stop run.

       check-colision section.
        *> check if the ball (square) collides with the top of the rectangle
        if (sx + ss + sxs is greater than or equal to tx)
           and (sx + sxs is less than or equal to tx + tw)
           and (sy + ss is less than or equal to ty)
           and (sy + ss + sys is greater than or equal to ty) then
         move ct to cc
         exit section
        end-if.

        *> check if the ball (square) collides with the bottom of the rectangle
        if (sx + ss + sxs is greater than or equal to tx)
           and (sx + sxs is less than or equal to tx + tw)
           and (sy is greater than or equal to ty + th)
           and (sy + sys is less than or equal to ty + th) then
         move cb to cc
         exit section
        end-if.

        *> check if the ball (square) collides with the left of the rectangle
        if (sy + ss + sys is greater than or equal to ty)
           and (sy + sys is less than or equal to ty + th)
           and (sx + ss is less than or equal to tx)
           and (sx + ss + sxs is greater than or equal to tx) then
         move cl to cc
         exit section
        end-if.

        *> check if the ball (square) collides with the right of the rectangle
        if (sy + ss + sys is greater than or equal to ty)
           and (sy + sys is less than or equal to ty + th)
           and (sx is greater than or equal to tx + tw)
           and (sx + sxs is less than or equal to tx + tw) then
         move cr to cc
         exit section
        end-if.

        move cn to cc.
        exit section.
