(declare-project
  :name "vigilant ducky"
  :description "an experimental game built with Janet and Raylib"
  :dependencies ["https://github.com/janet-lang/jaylib.git"
                 # "https://github.com/janet-lang/spork.git"
                 ])

(declare-executable
  :name "vigilant-ducky"
  :entry "game.janet")
