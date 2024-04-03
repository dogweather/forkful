---
date: 2024-01-26 04:13:13.245625-07:00
description: "Jak u\u017Cywa\u0107: Zacznij od uruchomienia REPL."
lastmod: '2024-03-13T22:44:34.996564-06:00'
model: gpt-4-0125-preview
summary: Zacznij od uruchomienia REPL.
title: Korzystanie z interaktywnego shella (REPL)
weight: 34
---

## Jak używać:
Zacznij od uruchomienia REPL:

```Clojure
user=> (println "Witaj, REPL!")
Witaj, REPL!
nil
```

Zdefiniuj funkcję i wypróbuj ją:
```Clojure
user=> (defn greet [name] (str "Witaj, " name "!"))
#'user/greet
user=> (greet "Programista Clojure")
"Witaj, Programista Clojure!"
```

Eksperymentuj ze strukturami danych:
```Clojure
user=> (def my-map {:a 1 :b 2})
#'user/my-map
user=> (assoc my-map :c 3)
{:a 1, :b 2, :c 3}
```

## W głąb tematu
REPL jest kluczowy dla filozofii interaktywnego rozwoju rodziny języków Lisp, a Clojure, nowoczesny dialekt Lispa, świetnie wykorzystuje to narzędzie. Jego początki sięgają pierwszego REPL Lispa pod koniec lat 50. Alternatywy w innych językach obejmują interpreter Pythona i konsolę Node.js, ale REPL dla Clojure ma status pierwszorzędny i jest integralną częścią przepływu pracy.

Sesja REPL w Clojure może być zintegrowana z różnymi środowiskami, takimi jak wiersz poleceń, IDE (na przykład IntelliJ z Cursive, lub Emacs z CIDER) lub narzędzia oparte na przeglądarce, takie jak Nightcode. W głębszym sensie, REPL umożliwia programiście manipulację konstruktami języka w czasie rzeczywistym oraz przenoszenie stanów przez różne transformacje, co często prowadzi do eksploracyjnego programowania i bardziej solidnego kodu.

Funkcjonalność REPL-a świeci przy użyciu narzędzi takich jak `lein repl` czy `clj`, które pozwalają na zarządzanie zależnościami, różnorodne wtyczki oraz dostosowania specyficzne dla projektu, prowadząc do bardziej produktywnego i elastycznego procesu rozwoju.

## Zobacz również
- Oficjalny przewodnik po stronie Clojure na temat REPL: https://clojure.org/guides/repl/introduction
- Wykład Richa Hickey'a na temat rozwoju opartego na REPL: https://www.youtube.com/watch?v=Qx0-pViyIDU
- Praktyczny Clojure: użycie REPL do iteracyjnego rozwoju: http://practicalclj.blogspot.com/2009/10/using-clojure-repl.html
