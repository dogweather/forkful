---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:30:10.285894-07:00
description: "Hur man g\xF6r: Clojure, som utnyttjar JVM, st\xF6djer olika testramverk.\
  \ Dock \xE4r ett vanligt anv\xE4nt inbyggt bibliotek `clojure.test`. H\xE4r \xE4\
  r ett enkelt exempel."
lastmod: '2024-03-13T22:44:37.528940-06:00'
model: gpt-4-0125-preview
summary: "Clojure, som utnyttjar JVM, st\xF6djer olika testramverk."
title: Skriva tester
weight: 36
---

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
