---
title:                "Slette tegn som matcher et mønster"
date:                  2024-01-20T17:42:05.708647-07:00
model:                 gpt-4-1106-preview
simple_title:         "Slette tegn som matcher et mønster"

category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/clojure/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Å slette tegn som matcher et mønster innebærer å finne og fjerne spesifikke tegnsekvenser fra en streng. Programmerere gjør dette for å rense data, forenkle tekst eller forberede strenger for videre prosessering.

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
