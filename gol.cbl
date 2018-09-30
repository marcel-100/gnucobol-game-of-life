       IDENTIFICATION DIVISION.
       PROGRAM-ID. game_of_life.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 cell_color    PIC 9.
       01 cell_random   PIC 9.

       01 counter       PIC 99.

       01 height        PIC 99 VALUE 50.
       01 height_max    PIC 99 VALUE 50.
       01 width         PIC 99 VALUE 50.
       01 width_max     PIC 99 VALUE 50.

       01 i             PIC 99.
       01 j             PIC 99.
       01 k             PIC 99.
       01 l             PIC 99.

       01 x             PIC 99.
       01 y             PIC 99.

       01 x_2           PIC 999.

       01 start_x       PIC 99.
       01 start_y       PIC 99.

       01 neighbours    PIC 9.
       01 random_seed   USAGE COMP-1.
       01 random_color  PIC 9.
       01 sleep_nano_s  PIC 9(10) VALUE 100000000. *> 0
       01 text_line     PIC X(50).

       01 world_real.
         02 w_y         OCCURS 50 TIMES.
           03 w_x       OCCURS 50 TIMES.
             04 cell    PIC 9.
              88 dead   value 0.
              88 alive  value 1,2,3.

       01 world_copy.
         02 w_y         OCCURS 50 TIMES.
           03 w_x       OCCURS 50 TIMES.
             04 cell    PIC 9.

       PROCEDURE DIVISION.
      * -------------------------------------------------------------- *
       DISPLAY
         "Enter a map as lines of 0's and 1's via STDIN, or press "
         "enter to create a random map. Maps are limited to "
         width_max " x " height_max " cells. Exit with Control-C."

       PERFORM create_world_from_stdin

       IF w_y IN world_real(1) = SPACES THEN
         PERFORM create_world_random
       END-IF

       PERFORM main_loop

       GOBACK
       .

      * -------------------------------------------------------------- *

       create_world_random SECTION.

       MOVE FUNCTION RANDOM(FUNCTION CURRENT-DATE(8:9))
         TO random_seed

       PERFORM VARYING y FROM 1 UNTIL y > height
         PERFORM VARYING x FROM 1 UNTIL x > width

           MOVE FUNCTION RANDOM TO random_seed
           COMPUTE cell_random ROUNDED = random_seed
           SET cell IN world_real(y, x) TO cell_random

         END-PERFORM
       END-PERFORM
       .

      * -------------------------------------------------------------- *

       create_world_from_stdin SECTION.

       PERFORM WITH TEST AFTER VARYING counter from 1 by 1
       UNTIL text_line = SPACES

         ACCEPT text_line FROM STDIN
         MOVE text_line TO w_y IN world_real(counter)

       END-PERFORM

       IF w_y IN world_real(1) NOT = SPACES THEN

         SUBTRACT 2 FROM counter GIVING height

         MOVE ZERO TO counter

         INSPECT w_y IN world_real(1) TALLYING counter FOR
           TRAILING SPACES

         SUBTRACT width_max FROM counter giving width

         IF height > height_max THEN
           MOVE height_max TO height
         END-IF

         IF width > width_max THEN
           MOVE width_max TO width
         END-IF

       END-IF
       .

      * -------------------------------------------------------------- *

       main_loop SECTION.

       MOVE FUNCTION CURRENT-DATE(15:1) TO random_color

       COMPUTE random_color = FUNCTION MOD(random_color, 7) + 1

       PERFORM FOREVER

         PERFORM display_world
         PERFORM create_next_generation

         CALL "CBL_GC_NANOSLEEP" USING sleep_nano_s

       END-PERFORM
       .

      * -------------------------------------------------------------- *

       display_world SECTION.

       PERFORM VARYING y FROM 1 UNTIL y > height
         PERFORM VARYING x FROM 1 UNTIL x > width

           COMPUTE x_2 = 2 * x - 1

           EVALUATE cell IN world_real(y, x)
             WHEN 0
               DISPLAY "  " AT COLUMN x_2, LINE y WITH
                 FOREGROUND-COLOR 0,
                 BACKGROUND-COLOR 0, REVERSE
             WHEN 1
               DISPLAY "  " AT COLUMN x_2, LINE y WITH
                 FOREGROUND-COLOR random_color,
                 BACKGROUND-COLOR random_color, REVERSE, HIGHLIGHT
             WHEN 2
               DISPLAY "  " AT COLUMN x_2, LINE y WITH
                 FOREGROUND-COLOR random_color,
                 BACKGROUND-COLOR random_color, REVERSE
             WHEN 3
               DISPLAY "  " AT COLUMN x_2, LINE y WITH
                 FOREGROUND-COLOR random_color,
                 BACKGROUND-COLOR random_color, REVERSE, LOWLIGHT
           END-EVALUATE

         END-PERFORM
       END-PERFORM
       .

      * -------------------------------------------------------------- *

       create_next_generation SECTION.

       PERFORM VARYING y FROM 1 UNTIL y > height
         PERFORM VARYING x FROM 1 UNTIL x > width

           PERFORM count_neighbours

           IF dead (y, x) THEN
             IF neighbours = 3 THEN
               SET cell IN world_copy(y, x) TO 1
             END-IF
           ELSE
             IF neighbours < 2 THEN
               SET cell IN world_copy(y, x) TO 0
             END-IF
             IF neighbours = 2 OR neighbours = 3 THEN
               EVALUATE cell IN world_real(y, x)
                 WHEN 1
                 WHEN 2
                   ADD 1 TO cell IN world_copy(y, x)
               END-EVALUATE
             END-IF
             IF neighbours > 3 THEN
               SET cell IN world_copy(y, x) TO 0
             END-IF
           END-IF

         END-PERFORM
       END-PERFORM

       MOVE world_copy TO world_real
       .

      * -------------------------------------------------------------- *

       count_neighbours SECTION.

       INITIALIZE neighbours

       SUBTRACT 1 FROM x GIVING i
       SUBTRACT 1 FROM y GIVING j

       PERFORM 3 TIMES
         PERFORM 3 TIMES

           MOVE i TO k
           MOVE j TO l

           PERFORM translate_coordinates

           IF i <> x OR j <> y THEN
             IF alive (l, k) THEN
               ADD 1 TO neighbours
             END-IF
           END-IF

           ADD 1 TO j

         END-PERFORM

         ADD 1 TO i

         SUBTRACT 1 FROM y GIVING j

       END-PERFORM
       .

      * -------------------------------------------------------------- *

       translate_coordinates SECTION.

       EVALUATE k
         WHEN 0
           MOVE height TO k
         WHEN (height + 1)
           MOVE 1 TO k
       END-EVALUATE

       EVALUATE l
         WHEN 0
           MOVE width TO l
         WHEN (width + 1)
           MOVE 1 TO l
       END-EVALUATE
       .

      * -------------------------------------------------------------- *

       END PROGRAM game_of_life.
