---
date: 2024-01-26 01:09:29.458108-07:00
description: "Jak to zrobi\u0107: Funkcje w Clojure definiujemy za pomoc\u0105 `defn`,\
  \ po kt\xF3rym nast\u0119puje nazwa, parametry i cia\u0142o funkcji. Oto szybki\
  \ przyk\u0142ad."
lastmod: '2024-03-13T22:44:35.001597-06:00'
model: gpt-4-1106-preview
summary: "Funkcje w Clojure definiujemy za pomoc\u0105 `defn`, po kt\xF3rym nast\u0119\
  puje nazwa, parametry i cia\u0142o funkcji."
title: Organizacja kodu w funkcje
weight: 18
---

## Jak to zrobić:
Funkcje w Clojure definiujemy za pomocą `defn`, po którym następuje nazwa, parametry i ciało funkcji. Oto szybki przykład.

```Clojure
(defn powitanie [imie]
  (str "Cześć, " imie "!"))

(powitanie "Alex") ; => "Cześć, Alex!"
```

Załóżmy teraz, że chcemy obliczyć pole prostokąta. Zamiast pakować wszystko do kupy, dzielimy to na dwie funkcje:

```Clojure
(defn pole [dlugosc szerokosc]
  (* dlugosc szerokosc))

(defn drukuj-pole [dlugosc szerokosc]
  (println "Pole wynosi:" (pole dlugosc szerokosc)))

(drukuj-pole 3 4) ; => Pole wynosi: 12
```

## Wgłębiając się
Dawno temu programiści wrzucali całą swoją logikę do jednego bloku. To było brzydkie. Potem przyszło programowanie strukturalne i funkcje stały się ważne. W Clojure każda funkcja jest klasą pierwszą – możesz nimi rzucać jak każdą inną wartością.

Alternatywy? Niektórzy mogą bawić się multi-metodami lub funkcjami wyższego rzędu, ale to tylko dodatki w zupie funkcji.

Wszystko w szczegółach funkcji: w Clojure są one niezmienne, co zmniejsza prawdopodobieństwo zamieszania przez efekty uboczne. Opierają się głównie na rekurencji zamiast typowych pętli, co dobrze współgra z funkcyjnymi paradygmatami języka.

## Zobacz również
- Przewodnik Clojure: https://clojure.org/guides/learn/functions
- Podstawy programowania funkcyjnego: https://www.braveclojure.com/core-functions-in-depth/
- Wykłady Richa Hickeya: https://changelog.com/posts/rich-hickeys-greatest-hits - dla wglądu w filozofię Clojure.
