---
title:    "Clojure: Konvertere en streng til små bokstaver"
keywords: ["Clojure"]
---

{{< edit_this_page >}}

## Hvorfor
Å konvertere en streng til små bokstaver kan være nyttig for å sammenligne eller søke etter tekst som er skrevet med ulike bokstavkombinasjoner.

## Hvordan gjøre det
For å konvertere en streng til små bokstaver, kan du bruke Clojure-funksjonen `lower-case` som tar inn strengen som et argument og returnerer den samme strengen med alle bokstavene i små bokstaver. Se et eksempel under:

```Clojure
(lower-case "HEI ALLE SAMMEN") ;; Output: hei alle sammen
```

## Dypdykk
Når man bruker funksjonen `lower-case` i Clojure, er det viktig å være obs på at den kun konverterer bokstaver som finnes i det engelske alfabetet. Dette betyr at bokstaver med aksenter eller andre spesialtegn vil forbli uendret. Det er også viktig å huske at funksjonen returnerer en ny streng og den originale strengen forblir uendret.

## Se også
- Clojure dokumentasjon for `lower-case`: https://clojuredocs.org/clojure.core/lower-case
- Mer informasjon om case-sensitivity i Clojure: https://clojure.org/guides/learn/evaluation#_case_sensitivity