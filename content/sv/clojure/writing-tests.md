---
title:                "Skriva tester."
html_title:           "Clojure: Skriva tester."
simple_title:         "Skriva tester."
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/clojure/writing-tests.md"
---

{{< edit_this_page >}}

# Varför

Att skriva tester är en viktig del av att skriva högkvalitativ kod. Det hjälper till att upptäcka och förebygga buggar, förbättrar koden och ger ökad tillit till applikationen.

# Hur man skriver tester

För att skriva tester i Clojure behöver vi använda en testram som heter `clojure.test`. Detta ger oss funktioner och makron för att definiera testfall och förväntade resultat.

Ett enkelt exempel skulle kunna vara att vi vill testa en funktion som lägger till två tal:

```Clojure
(ns testing.core-test
  (:require [clojure.test :refer [is]]))
 
(defn add [x y]
  (+ x y))

(deftest addition-test
  (is (= 5 (add 2 3))))
```

Vi börjar med att definiera en namnrymd för våra tester och inkluderar `clojure.test` för att använda `is`-funktionen. Sedan definierar vi funktionen vi vill testa, `add`, och skriver ett testfall för att kontrollera att det ger det förväntade resultatet, `5`.

För att köra testerna från en REPL eller terminal, kan vi använda följande kommando:

```Clojure
(clojure.test/run-tests)
```

Om allt är korrekt, bör vi få ut en grön markering för vårt testfall.

# Djupgående

När vi skriver tester vill vi täcka så många olika scenarier som möjligt. Detta inkluderar att testa olika indata, hörnfall och felhantering. Vi kan även använda oss av `testing`-makron för att organisera våra tester och ge dem en mer beskrivande rubrik.

```Clojure
(ns testing.core-test
  (:require [clojure.test :refer [testing is]]))
 
(defn divide [x y]
  (/ x y))

(deftest division-test
  (testing "Division av två positiva tal"
    (is (= 2 (divide 6 3))))
  (testing "Division av ett positivt och ett negativt tal"
    (is (= -2 (divide 6 -3))))
  (testing "Division av noll"
    (is (thrown? ArithmeticException (divide 6 0)))))
```

I det här exemplet täcker vi flera olika scenarier för funktionen `divide` - division av två positiva tal, division av ett positivt och ett negativt tal och division av noll. Vi använder `thrown?`-funktionen för att kontrollera att ett undantag kastas vid felaktig indata.

Det finns många fler funktioner och makron som kan användas vid skrivande av tester, till exempel `are`, `are-not` och `deftest+`. Det är också viktigt att komma ihåg att skriva klara och förståeliga testfall, så att andra utvecklare lätt kan förstå vad de testar.

# Se även

- [Clojure Dokumentation för `clojure.test`](https://clojure.github.io/clojure/clojure.test-api.html)
- [Clojure.test Tutorial](https://clojure.org/guides/testing)
- [Using clojure.test](https://blog.venanti.us/clojure-tdd)