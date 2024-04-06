---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:09:20.369190-07:00
description: "Hoe te gebruiken: Reguliere expressies hebben een rijke geschiedenis,\
  \ die teruggaat tot het theoretische werk van Stephen Cole Kleene in de jaren 1950.\u2026"
lastmod: '2024-04-05T21:53:50.434653-06:00'
model: gpt-4-0125-preview
summary: Reguliere expressies hebben een rijke geschiedenis, die teruggaat tot het
  theoretische werk van Stephen Cole Kleene in de jaren 1950.
title: Reguliere expressies gebruiken
weight: 11
---

## Hoe te gebruiken:
```clojure
(require '[clojure.string :as str])

;; 1. Matchen
(re-matches #"\d+" "123")               ;; => "123"
(re-matches #"\d+" "abc")               ;; => nil

;; 2. Zoeken
(re-find #"\d+" "Bestel 100 appels")     ;; => "100"

;; 3. Vervangen
(str/replace "2023-03-15" #"\d{4}" "YYYY") ;; => "YYYY-03-15"

;; 4. Splitsen
(str/split "een,twee,drie" #",")       ;; => ["een" "twee" "drie"]
```

## Diepere Duik
Reguliere expressies hebben een rijke geschiedenis, die teruggaat tot het theoretische werk van Stephen Cole Kleene in de jaren 1950. Alternatieven voor regex zijn stringfuncties zoals `indexOf`, `substring` en parsingbibliotheken; echter, regex biedt vaak een beknoptere oplossing. Clojure's regex-mogelijkheden zijn ingebouwd in Java's `Pattern` klasse, en bieden krachtige patroonmatching direct in de taal.

## Zie Ook
- [ClojureDocs over Reguliere Expressies](https://clojuredocs.org/clojure.core/re-find)
- [Java Pattern klasse](https://docs.oracle.com/javase/8/docs/api/java/util/regex/Pattern.html)
