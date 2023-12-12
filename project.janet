(declare-project
  :name "game"
  :description "a game"
  :dependencies ["https://github.com/janet-lang/jaylib.git"
                 # "https://github.com/janet-lang/spork.git"
                 ])

(declare-executable
  :name "game"
  :entry "game.janet")
