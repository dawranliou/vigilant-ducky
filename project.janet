(declare-project
 :name "example"
 :description "example"
 :dependencies ["https://github.com/janet-lang/jaylib.git"
               "https://github.com/janet-lang/spork.git"])

(declare-executable
  :name "example"
  :entry "example.janet")
