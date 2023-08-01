(load "generator.lisp")

(create-post "An Introduction"
  (heading 3 "Hello!")
  "My name is Alice, or Cassie, I like both"
  (paragraph 
    "I'm a " 
    (image "assets/transicon.png" "transgender flag")
    "trans"
    (image "assets/transicon.png" "transgender flag")
    " brazilian game dev and you're current on my personal website/blog!")
    "I'll probably post about whatever variety things i'm interested in/thinking about here"
    "A list of things that you might see here include, in no particular order:"
    (u-list
      "Game Development!"
      "Doctor Who!"
      "General Programming Stuff!"
      "Gay Stuff!"
      "Games i'm playing!"
      "And anything else that my brain decides to think about"))

(quit)
