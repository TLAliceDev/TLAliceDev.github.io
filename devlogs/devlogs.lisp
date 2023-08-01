(load "generator.lisp")

(setf *folder* #P"devlogs/")
(setf *index-path* "../index.html")
(setf *devlogs-path* "devlogs.html")
(setf *css-path* "../css/style.css")

(defvar *posts* '())

(defun new-devlog (title &rest content)
  (setf *posts* (append *posts* (apply #'create-post title content) '("<hr>"))))


(new-devlog "Symmages"
  "So, initially I was planning on making a double submission for the Godot Wild Jam and the GMTK Jam. But unfortunately I just didn't manage to think of a single idea which fitted both the \"Symmetry\" theme from the GWJ and the \"Roll of the dice\" theme from the GMTK, and ended up stuck. Early in the last day of both jams I ended up deciding to give up on both of them."
  "But, just 2 hours and 5 minutes before the end of the GWJ, Vela on its discord mentioned a quick game idea, a fighting game where your enemy mirrors your movement with a slight delay. I decided to try making it with their permission in the remaining time of the jam and immediately got to work."
  "My first step was gathering some art assets since there was no way I would be able to make the ones I need and still have the time to make the game. I ended up going with the 1-Bit Asset Packs from Kenney. As soon as I had the assets imported in the engine I started making the player character. It was a very simple 2D Platformer controller, with some simple polish features like Jump Buffering, Coyote Time and Variable Jump Height, the latter of which was a bit of a pain to implement. "
  "After getting the player controller done, I designed a quick arena for the fighting to happen. It was an extremely simple and boring design. If I had more time to finish the game, I'd definitely have added map hazards and made some arena variations to avoid the boringness and to also deal with some later design issues."
  "When the arena was done and the player running around, I went on to code the enemy. Due to time constraints, the enemy/player code went in the same script, and it was a bit of a mess as you can imagine. The enemy worked by appending all of the player inputs in the frame a certain format to a queue. And then after 0.25 seconds had passed, the enemy would start processing the input from that queue in the same way as the player, making it so that every player movement was mirrored by the enemy with a 0.25s delay."
  "By this point I was running into a few issues with the animation system. I rarely ever use Godot for 2D games, so I wasn't familiar with the way AnimatedSprite worked, and fixing said issues probably took around 10 precious minutes off my development time. After I got them fixed, I moved on to coding the attack system. I was initially planning on having a melee combat system, but the lack of animations for that in the asset pack I was using + the time constraint made me decided to go with a simple shooting one instead. It was very simple to code, but I ran into the issue that, since the player always shot 0.25 seconds before the enemy, then if the player and the enemy just went to the center of the map and shot, then the enemy would always die before having the chance to shoot back at the player. To fix that, I added a short timer in the shot spell code, and made it so that it'd only kill the hit target if that timer was over. I wanted to contextualise that as a sort of \"small-range shield spell\", but realised I wouldn't have the time to code in the effects necessary for that context unfortunately."
  "After making it so that, when any of the mages got hit by a spell, they died and the game reset after a short delay. I went on to add sound effects and music. All of the SFX were generated using the SFXR tool, and I got the music from opengameart.org. After quicky importing all of the sound assets and coding them to play under the correct circumstances, I added some small particle effects when a mage died. After that was done, I coded a simple Scoring system for keeping track of how many times the player had killed the enemy and vice-versa, displayed them with just a Label node using a pixel font by Kenney, and the game was Done."
  "Looking back on it, there were a lot of improvements that could be made to the game. The main one being ways of making it so that the player cannot just repeat the same set of simple actions to easily cheese the game. The main way of fixing that that I thought of was map variation and hazards. While the game is most definitely Not Good™, I'd say I'm generally happy with what I managed to get done in the strict timeframe."
  (image "Symmages.gif" "gif of the game"))

(new-devlog "Blisp"
  (heading 4 "Making a Lisp-based Static Site Generator")
  "When i decided to actually start using this site, i also decided to make a static site generator to make writing posts for it easier."
  (paragraph "The vast majority of SSGs out there obviously use Markdown for writing their posts. It makes sense, it's a good, well known format that is great for this stuff, "
	     (bold "any smart person would obviously choose to use it."))
  (line-break)
  "So instead i obviously chose to write a Common Lisp program with functions that format text into various HTML tags and then puts them all together into a final web page"
  (hr)
  "My primary motivation for doing so was mostly just the fact that i like Lisp and wanted an excuse to use it. I feel like if i tried hard enough i could come up with some excuses as possible advantages that doing things this way instead of just markdown might bring, but i cannot think of any at the moment"
  "As to how this works:"
  "I have a series of functions such as (PARAGRAPH), (BOLD), (IMAGE) that take in some arguments (usually just a string of text) and returns them packed properly inside of their corresponding HTML tags, for example:"
  "(bold \"This Text is Bold \" (italics \"And This One Is Also Italic\"))"
  "returns the string \"<b>This Text is Bold <i>And This One Is Also Italic</i></b>"
  (hr)
  "For putting them together into a final page, certain parameters, such as a template page and mandatory paths, are given through variables, and Blisp then searches for certain keywords in the template page and replaces them with the correct element."
  "For example, the $$TITLE$$ keyword gets replaced with the page title, which is passed to the (CREATE-POST) function"
  (hr)
  "A fun aspect of this process is that it easily allows me to create pages that themselves create other pages. For example, this very page's source code is inside of the source code that generates every page for the Devlogs section of the blog, and then automatically generates the devlogs.html page with links to individual devlogs itself"
  (hr)
  "In a little bit of meta-fun, the source code for this current page is displayed below:"
  (image "blisp_page_code.png" "A nvim screenshot showcasing the source code used to generate the page you're currently on. Most of the text is the exact same as above"))


(apply #'create-post "devlogs"
  *posts*)
  
  

(quit)
