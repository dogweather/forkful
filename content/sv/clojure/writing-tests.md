---
title:                "Clojure: Att skriva tester"
programming_language: "Clojure"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/clojure/writing-tests.md"
---

{{< edit_this_page >}}

## Varför

Att skriva tester är ett viktigt steg i utvecklingen av robusta och pålitliga program. Genom att skapa tester kan du säkerställa att din kod fungerar som avsett och minska risken för buggar och fel i produktion.

## Hur man gör det

För att skriva tester i Clojure behöver du först och främst en förståelse för funktionell programmering, då Clojure är ett funktionellt språk. Ett enkelt exempel på ett funktionellt test är att skapa en funktion som tar in två tal och returnerar deras summa. 

```Clojure
(defn sum [a b]
  (+ a b))

(sum 2 2) ; => 4
```

Här kan vi sedan skapa ett test för att säkerställa att funktionen alltid returnerar rätt summa:

```Clojure
(deftest test-sum
  (is (= 4 (sum 2 2)))
  (is (= 10 (sum 5 5))))

(run-tests) ; => Test passes
```

Genom att använda assert-funktionen `is` och jämföra resultatet av `sum` med det förväntade värdet kan vi enkelt testa vår funktion.

## Djupdykning

Att skriva tester handlar inte bara om att bekräfta att koden fungerar, utan också om att förbättra din kod. Genom att skriva tester tvingas du att tänka på alla möjliga scenarier och undvika potentiella buggar i framtiden. Dessutom blir det mycket lättare att upptäcka fel och buggar när du har en uppsättning tester som kontrollerar din kod regelbundet.

## Se även

- [ClojureDocs](https://clojuredocs.org)
- [Clojure for the Brave and True](https://www.braveclojure.com/)
- [Test-Driven Development i Clojure](https://medium.com/@startupjs/test-driven-development-in-clojure-8096d4ebb1d4)