---
title:                "Skriva tester"
html_title:           "Arduino: Skriva tester"
simple_title:         "Skriva tester"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/clojure/writing-tests.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Skriva tester innebär att skapa små program som kontrollerar att din kod fungerar som förväntat. Programmerare gör det för att upptäcka buggar snabbt, förenkla framtida förändringar och säkerställa att programmet fortsätter att fungera korrekt genom olika utvecklingsstadier.

## Hur gör man?
Clojure använder `clojure.test` för att skriva tester. Här är ett exempel på en enkel test-funktion och dess output:

```Clojure
(require '[clojure.test :refer :all])

(deftest addition-test
  (testing "Addition fungerar"
    (is (= 5 (+ 2 3)))))

(run-tests)

; Test output:
;=> 
;Ran 1 tests containing 1 assertions.
;0 failures, 0 errors.
```

Detta exempel definierar ett test för en enkel addition och kör detta med `run-tests`.

## Fördjupning
Historiskt sett har testning i programmering utvecklats från manuell granskning till automatiserade system som det i Clojure. Alternativ till `clojure.test` inkluderar ramverk som `Midje` och `test.check`, som erbjuder olika förhållningssätt och förbättrade funktioner. När du skriver tester är det viktigt att fokusera på testfallens relevans snarare än kvantitet för att effektivt täcka kritiska aspekter av koden.

## Se även
- Clojure's officiella guide för `clojure.test`: https://clojure.github.io/clojure/clojure.test-api.html
- Midje på GitHub: https://github.com/marick/Midje
- Användbar guide till Property-based testing med `test.check`: https://github.com/clojure/test.check
