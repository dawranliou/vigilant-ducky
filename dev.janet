#! /usr/bin/env janet
(def DEV true)
(setdyn *redef* true)
(setdyn *debug* true)
(import spork/netrepl)
(use ./game)
(netrepl/server-single "127.0.0.1" "9365" (fiber/getenv (fiber/current)))
