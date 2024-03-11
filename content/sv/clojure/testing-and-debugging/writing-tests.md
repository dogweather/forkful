---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:30:10.285894-07:00
description: "Att skriva tester i Clojure, precis som i andra programmeringsspr\xE5\
  k, inneb\xE4r att skapa s\xE4rskild kod f\xF6r att verifiera att din huvudkod fungerar\
  \ som\u2026"
lastmod: '2024-03-11T00:14:10.855005-06:00'
model: gpt-4-0125-preview
summary: "Att skriva tester i Clojure, precis som i andra programmeringsspr\xE5k,\
  \ inneb\xE4r att skapa s\xE4rskild kod f\xF6r att verifiera att din huvudkod fungerar\
  \ som\u2026"
title: Skriva tester
---

{{< edit_this_page >}}

## Vad & Varför?
Att skriva tester i Clojure, precis som i andra programmeringsspråk, innebär att skapa särskild kod för att verifiera att din huvudkod fungerar som förväntat. Det hjälper till att säkerställa korrekthet, underlätta refaktorering och förbättra kodstabiliteten.

## Hur man gör:
Clojure, som utnyttjar JVM, stödjer olika testramverk. Dock är ett vanligt använt inbyggt bibliotek `clojure.test`. Här är ett enkelt exempel:

```clojure
(ns example.test
  (:require [clojure.test :refer :all]
            [example.core :refer :all]))

(deftest test-addition
  (testing "Additionsfunktionalitet"
    (is (= 4 (add 2 2)))
    (is (= 7 (add 3 4)))))

(run-tests)
```
Efter att ha kört detta test skulle du se en utskrift liknande:

```
Testar example.test

Körde 2 tester innehållande 2 påståenden.
0 misslyckanden, 0 fel.
```

För de som söker mer funktionsrika alternativ kan man använda tredjepartsbibliotek som `Midje` eller `test.check`. Så här kan du använda Midje för ett liknande test:

Först, lägg till Midje i dina projekt.clj beroenden:
```clojure
[midje "1.9.9"]
```

Sedan kan ditt test med Midje se ut så här:

```clojure
(ns example.test
  (:require [midje.sweet :refer :all]
            [example.core :refer :all]))

(fact "Testar addition"
  (add 2 2) => 4
  (add 3 4) => 7)
```

När du kör testet genom Midje med `lein midje`, skulle utskriften visa något liknande:

```
Alla kontroller (2) lyckades.
```
