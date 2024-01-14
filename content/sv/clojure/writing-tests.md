---
title:    "Clojure: Att skriva tester"
keywords: ["Clojure"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/sv/clojure/writing-tests.md"
---

{{< edit_this_page >}}

## Varför

Att skriva tester är en viktig del av att skriva bra kod. De hjälper till att hitta buggar och säkerställa att koden fungerar som den ska. I denna bloggpost kommer jag att visa hur man skriver tester i Clojure och varför det är en viktig del av utvecklingsprocessen.

## Hur man skriver tester i Clojure

För att skriva tester i Clojure behöver du ett testramverk som heter "clojure.test". Det är en del av Clojure standardbibliotek och är enkelt att använda. För att börja skriva tester behöver du bara importera "clojure.test" biblioteket och definiera en funktion med namnet "test" och sedan skriva dina tester inuti den.

```Clojure
(ns test-projekt
  (:require [clojure.test :refer [deftest is]])

(deftest min-funktion-test
  (is (= 5 (+ 2 3)))
  (is (not= 1 2))))
```

Som du kan se i exemplet ovan så använder vi funktionen "deftest" för att definiera våra tester och vi använder funktionen "is" för att kolla om ett uttryck evaluerar till sant. Om testet misslyckas så får du ett tydligt felmeddelande med information om var testet misslyckades.

## Djupdykning

När du skriver tester är det viktigt att täcka så många fall som möjligt för att få tillförlitliga tester. Du kan använda "is" funktionen för att kontrollera om ett uttryck är sant men det finns även andra funktioner som du kan använda beroende på vilken typ av data du förväntar dig.

```Clojure
(is (= 5 (+ 2 3))) ; jämför om två värden är lika
(is (not= 1 2)) ; jämför om två värden är olika
(is (<= 5 6)) ; kollar om ett värde är mindre än eller lika med ett annat
(is (> 10 5)) ; kollar om ett värde är större än ett annat
(is (contains? {:a 1 :b 2} :a)) ; kollar om en nyckel finns i en map
```

Du kan även använda "is" för att kontrollera exceptioner och asynkron kod vilket är viktigt för att säkerställa att din kod fungerar som den ska i alla situationer.

## Se även

Här är några användbara länkar för att lära dig mer om att skriva tester i Clojure:

- [Officiell dokumenation för clojure.test](https://clojure.github.io/clojure/clojure.test-api.html)
- [Tutorial om att skriva tester i Clojure](https://vlaaad.github.io/clojure/test/tdd/2015/11/03/clojurescript-intro#toc9)
- [En djupgående guide om att skriva tester i Clojure](https://yogthos.net/posts/2014-08-15-Testing-in-Clojure.html)