---
title:                "Slette tegn som samsvarer med et mønster"
html_title:           "Arduino: Slette tegn som samsvarer med et mønster"
simple_title:         "Slette tegn som samsvarer med et mønster"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/clojure/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
"Sletting av tegn som samsvarer med et mønster" er prosessen med å finne og fjerne karakterer i en streng som passer til et definert sett eller sekvens. Dette brukes av programmerere til å rense data, foreta tekstbehandlingsoppgaver eller redusere redundanse i strengverdier.

## Hvordan gjøre det?
Legg merke til følgende eksempler:

```Clojure
(use '[clojure.string :only [replace]])

(defn delete-chars-matching-pattern
  [s pattern]
  (replace s pattern ""))

(def hilsen "Hei, dette er eksempeltekst!")
(delete-chars-matching-pattern hilsen #"eksempel") ; => "Hei, dette er tekst!"
```
Her bruker vi funksjonen `replace` fra `clojure.string`. `#"eksempel"` er et regulært uttrykk som definerer mønsteret vi vil fjerne. Resultatet sletter orde 'eksempel' fra hilsenen.

## Dypdykk
(1) Historisk kontekst: Funksjonen `replace` har vært en del av Clojure biblioteket siden utgivelsen sin i januar 2009. Å ekskludere/inkludere karakterer basert på mønstre er et grunnleggende konsept fra tidlige dager av tekstbehandling og programmering.

(2) Alternativer: I stedet for `replace`, kan du også bruke `clojure.string/replace-first` hvis du bare vil erstatte første forekomst av mønsteret. En annen metode er API'et i `java.util.regex`, som har mer avanserte funksjoner for regulære uttrykk.

(3) Implementeringsdetaljer: `replace` er implementert ved bruk av Java String's `replaceAll` metode. Det er viktig å merke seg at den returnerer en ny streng i stedet for å endre den originale teksten, som er i tråd med Clojure sin filosofi om uforanderlighet.

## Se Også
1. [Clojure string Replace documentation](https://clojuredocs.org/clojure.string/replace)
2. [Java regex Pattern documentation](https://docs.oracle.com/javase/8/docs/api/java/util/regex/Pattern.html)
3. [Clojure for the Brave and True: Regular Expression](https://www.braveclojure.com/core-functions-in-depth/)