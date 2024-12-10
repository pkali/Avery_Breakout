# Avery Breakout
A very break out game. Name is a homage to Mr. Avery Lee, also known as [phaeron](https://forums.atariage.com/profile/16457-phaeron/)

Thank you Avery for everything you are doing for this small community. 

I have shown engine protos before, but it was not a real game, just a few bouncing balls.

[Pecus](https://github.com/Pecusx) for the rescue! In a couple of evenings we were able to transform it into a (hopefully) playable game.

Music and SFX by Alex, his first RMT attempt :)

Font by [DamienG](https://damieng.com/typography/zx-origins/mild-west/)


The `backup` folder contains easier to play hi-res, b-w version. 


## 2024-12-07
We teamed with Pecus and Alex and made a game out of the "tech demo". There are points, game over screen and possibility to add your own levels.
If there is no DOS, you can play the single built-in level.
With DOS, the game tries to load `LEVELnnn.DAT` file, starting from `LEVEL001.DAT` from the current directory.
The levels can be edited on Atari with the enclosed `ED.COM` editor or a PC (end line characters can be CR, LF, CRFL or ATASCII EOL).

### Level file format:
line 1: number of balls to be hit for the level to succeed
line 2: `1` means single pixel width of the pattern, `2` double pixel width
the following lines define the level. Any character is a pixel, space is a space, EOL is the end of line.

The game displays 46 lines of pixels, but your levels should generally be shorter to make it playable.

Experiment!


## 2020-05-13
Second place in the game compo in PSA 2020 virtual SARS-2 demo party.


### TPS Report:

## 2012-02-16
Fork to the colour version. (averybreakout.asm) Graphics mode tests.


## build 023: 2010-06-26

      + idle ball delay loop shortened to 75 loops for a smoother experience 

T027: when ball is down out of the screen and the racquette is over it, it bounces. 
      Need to add a check for it! (no bounces when out of the screen)

## build 022: 2010-06-24
T022: when balls disappear, their traces should disappear, too, even when a new ball is NOT created
      now the dead pixels disappear only when a new ball is created.
      It has been done by expanding the bottom area, so the balls are going down.
      Screen had to been cut 10 lines. Not a big deal.

T024: detect the ending
      Detected (gameIsNotOver label).


## build 021: 2010-06-21
+ apparently the slowest part of the engine is eXistenZcheck - rewrite as a simple stack!
  as usual, it was not as simple as thought previously... But works now!
  Will be easier to clear traces of the dead balls now.

## build 020: 2010-06-02
+ when the racquette is max to the right it does not bounce balls!
+ still problems with racquet
  solved - pos+size was >$ff
+ very fast balls get through the left-right side borders
  solved by a better usage of "maxSpeed"

## build 019: 2010-05-27
+ alive balls are somewhere in the outer space - check where and fix
    where: YposTableH -- 00 -- 08 most 01, XposTableH - FF, FE, 00, 01
    basicaly these are places where a ball should never be!
  One fix let other "outer-space" related buggies die, too.
  The problem was that bouncing ball got behind the border and started to bounce
  there and forth outside the screen. Fix - bring it back on the playfield.
+ balls are created somewhere outside the screen
+ when balls disappear, their traces should disappear, too (now 1 pixel stays on screen)
+ high dX balls were sticking to Vborders. Fixed by increasing the margin

## build 018: 2010-05-26
+ 1 pixel out of the deleted bunch stays forever (erase/store sequence was invalid)

build 017: 2010-05-25
+ too few high dX balls
x low-priority: rewrite memorytables to use lda (zpage,x) addressing (cool:)
    Turned out not to be such a low-priority job as wrong sequence of writes
    to memorytables make one pixel staying on the screen. Rewrite to simplify!
    Turned out that indirect X addressing is not good for it and this is closed.

## build 016: 2010-05-24
Forgot about documenting updates...
Basically the game is close to the end, but number of small issues emerged.
+ racquette is too slow (and it was such a work to make it 1-px smooth...)

## build 006: 2010-05-12
Collision detection
Ough... turned out to be way more difficult than expected, but now collisions are detected and balls are bouncing!

## build 005: 2010-05-10
Snake like plot memory! Lots and lots of pixels, less balls (64 is the max...)

## build 004: 2010-05-09
Multiple ballz

## build 003: 2010-05-08
Feeling bouncy
simple boundary bounces work nicely.
Unfrtunately number of draws per frame dropped to circa 70.
I guess 64 will be an achievement for 2  frames.

## build 002: 2010-05-06
MADS rewrite :-]
;bare plots get circa 320 pixels per frame. I will go for 128 pix max now

## build 001: 2010-05-04

First try:
Strip scorch sources to get clean gr.8 screen.
(and PLOT)

Basic idea: http://wonderfl.net/c/tNGi/fullscreen


### TODO: (old)

T021: balls are ferking stick to the right part of the screen...

T023: find and set few nice starting points and speeds to be rotated (remove not really sexy random start)

T025: do nice game over (when not clear)

T026: do nice "ALL CLEAR" screen

T028: (idea) spinning the ball - when ball bounces the racquette when it is moving, dX adds or subs
