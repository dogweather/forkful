---
title:                "Att skriva tester"
html_title:           "Clojure: Att skriva tester"
simple_title:         "Att skriva tester"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/clojure/writing-tests.md"
---

{{< edit_this_page >}}

Vad & Varför?
Att skriva tester är en viktig del av programmering. Det handlar om att skriva kod som verifierar att programmet fungerar som det ska. Tester hjälper oss att upptäcka buggar och fel i koden, och ger oss mer tillförlitliga program.

Hur gör man?
I Clojure använder vi oss av biblioteket `clojure.test` för att skriva tester. Detta ger oss en mängd funktioner som vi kan använda för att skriva olika typer av tester. Här är ett exempel på hur man skulle kunna skriva ett enkelt test:

```clojure
(ns example-test
  (:require [clojure.test :refer :all]))

;; Vi skapar en funktion som vi vill testa
(defn multiply [x y]
  (* x y))

;; Vi skriver ett test för funktionen
(deftest test-multiply
  (testing "Multiplicera två positiva tal"
    (is (= 24 (multiply 8 3)))))

;; Kör alla tester i detta namespace
(run-tests)
```

Resultatet av testet bör bli:

```clojure
Testing example-test

Ran 1 tests containing 1 assertions.
0 failures, 0 errors.
```

Djupdykning
Historiskt sett har testning varit en viktig del av programmering. En vanlig metod var att manuellt testa programmet och se om det fungerade som förväntat. Detta var dock väldigt tidskrävande och ineffektivt. Med introduktionen av automatiserade tester har processen blivit mycket smidigare och tillförlitligare.

En alternativ metod för testning i Clojure är användning av biblioteket `clojure.spec`. Detta ger oss möjlighet att specificera vilken sorts data som funktioner tar emot och returnerar, och låter oss sedan köra genomsöknings- och valideringsfunktioner för att säkerställa att funktionerna uppfyller specifikationerna.

Se också
- https://clojure.org/guides/testing
- https://clojure-doc.org/articles/tutorials/testing.html