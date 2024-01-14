---
title:                "Clojure: Att skriva tester"
simple_title:         "Att skriva tester"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/clojure/writing-tests.md"
---

{{< edit_this_page >}}

##Varför##

Att skriva tester i Clojure är en viktig del av att utveckla en pålitlig och robust kodbas. Genom att skriva tester kan du säkerställa att din kod fungerar som den ska och att eventuella förändringar inte leder till oväntade buggar eller buggar som tidigare fungerade kod. Dessutom kan det hjälpa till att identifiera problem i koden tidigt, vilket sparar tid och resurser på lång sikt.

##Så här##

För att skriva tester i Clojure använder du biblioteket "clojure.test", som är en del av standardbiblioteket. Det första steget är att importera det i ditt projekt:

```Clojure
(ns my-project.core-test
  (:require [clojure.test :refer [deftest is]]))
```

För att skapa ett test använder du funktionen "deftest" och ger det ett unikt namn som beskriver vad testet testar:

```Clojure
(deftest my-addition-test
  ...)
```

Inuti testet kan du använda funktionen "is" för att göra påståenden om din kod:

```Clojure
(deftest my-addition-test
  (is (= (+ 2 2) 4))
  (is (= (+ 2 3) 5)))
```

När du är klar med dina tester kan du köra dem genom att kalla på funktionen "run-tests" i din testfil:

```Clojure
(defn -main []
  (run-tests))
```

Det här kommer att köra alla dina tester och visa resultatet av dem.

##Djupdykning##

När du skriver tester i Clojure kan du använda ett antal olika funktioner för att göra påståenden om din kod. Här är några av de vanligaste:

- "is" - Kontrollerar om två uttryck är lika.
- "is-not" - Kontrollerar om två uttryck inte är lika.
- "throws?" - Kontrollerar om ett uttryck kastar ett undantag.
- "throws-with-msg?" - Kontrollerar om ett undantag kastas med ett visst meddelande.
- "throws-with-cause?" - Kontrollerar om ett undantag kastas med en viss orsak.

För mer information och exempel, se Clojure-testets dokumentation [här](https://clojure.github.io/clojure/clojure.test-api.html).

##Se även##

- [Clojure-testets dokumentation](https://clojure.github.io/clojure/clojure.test-api.html)
- [The Clojure Cookbook - Writing Tests](https://github.com/clojure-cookbook/clojure-cookbook/blob/master/14_testing/14_03_writing_tests.adoc)
- [Testing in Clojure - A Beginner's Guide](https://blog.usejournal.com/testing-in-clojure-a-beginners-guide-891d92810c3a)