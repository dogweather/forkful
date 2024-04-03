---
date: 2024-01-20 17:42:05.708647-07:00
description: "\xC5 slette tegn som matcher et m\xF8nster inneb\xE6rer \xE5 finne og\
  \ fjerne spesifikke tegnsekvenser fra en streng. Programmerere gj\xF8r dette for\
  \ \xE5 rense data,\u2026"
lastmod: '2024-03-13T22:44:40.387155-06:00'
model: gpt-4-1106-preview
summary: "\xC5 slette tegn som matcher et m\xF8nster inneb\xE6rer \xE5 finne og fjerne\
  \ spesifikke tegnsekvenser fra en streng."
title: "Slette tegn som matcher et m\xF8nster"
weight: 5
---

## Hvordan gjøre det:
For å slette tegn som matcher et mønster i Clojure, kan du bruke `clojure.string/replace` funksjonen med et regulært uttrykk.

```Clojure
(require '[clojure.string :as str])

; Eksempel: Fjerne alle vokaler fra en streng
(defn delete-vowels [s]
  (str/replace s "[aeiouAEIOU]" ""))

; Bruk
(delete-vowels "Hei Verden") ; => "H Vrdn"
```

```Clojure
; Eksempel: Fjerne alle tall fra en streng
(defn delete-digits [s]
  (str/replace s "\\d+" ""))

; Bruk
(delete-digits "Clojure 2023") ; => "Clojure "
```

```Clojure
; Eksempel: Fjerne spesialtegn, unntatt mellomrom
(defn delete-special-chars [s]
  (str/replace s "[^a-zA-Z0-9 ]" ""))

; Bruk
(delete-special-chars "Hello, Verden!") ; => "Hello Verden"
```

## Dykk dypere
I eldre dager var tekstmanipulasjon kronglete. Med moderne programmeringsspråk som Clojure er det enkelt. `clojure.string/replace` er en kraftig funksjon som bruker Java’s `Pattern` klasse under hetten for effektivitet. Alternativer inkluderer manuell iterasjon over strenger eller bruk av biblioteker som `clojure.spec` for mer kompleks validering og transformasjon.

Regulære uttrykk, som vi bruker her, er kompakte, men kan være kryptiske. Viktig: de matcher mønstre, ikke spesifikke tegn. Å forstå dem krever praksis.

## Se også
- [Clojure String API Docs](https://clojuredocs.org/clojure.string/replace)
- [ClojureDocs – et samfunnsdrevet Clojure-cheat sheet](https://clojuredocs.org/)
