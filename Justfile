build:
    clojure -T:build jar

release:
    clojure -T:build release
