---
title:                "Søking og erstatning av tekst"
html_title:           "Lua: Søking og erstatning av tekst"
simple_title:         "Søking og erstatning av tekst"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/clojure/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

---

# Hvordan søke og erstatte tekst i Clojure: En ikke-verbose veiledning

## Hva & Hvorfor?

Å søke og erstatte tekst i programmering henviser til prosessen med å identifisere bestemte tegnsekvenser (strenger) og erstatte dem med andre. Programmerere gjør dette for å manipulere data, rense innspill, eller transformere tekst på effektive måter.

## Hvordan Det Gjøres:

I Clojure bruker vi funksjonen `clojure.string/replace` for å søke og erstatte tekst. Her er noen eksempler:

```Clojure
(require '[clojure.string :as str])
(defn search-and-replace 
  [text find replace-with]
  (str/replace text find replace-with))

(search-and-replace "Hei, Verden!" "Verden" "Clojure")
; => "Hei, Clojure!"
```

## Dypdykk: 

Clojures innebygde strengmanipulasjonsfunksjoner kommer fra sin Lisp-arv. Før Clojure, måtte Lisp og andre programmeringsspråk vanligvis stole på regulære uttrykk for tekstmanipulasjon.

`clojure.string/replace`-funksjonen er relativt enkel, men den er veldig kraftig. Den kan ta enten en streng, et tegn, eller en regex som sin "finn" -parameter, noe som gir enorme muligheter for søke- og erstatningsoperasjoner. På grunn av dens enkelhet, har Clojure ingen innebygde alternativer for `clojure.string/replace`, men det er mange tredjepartsbiblioteker som tilbyr mer komplekse tekstbehandlingsfunksjoner hvis det trengs.

## Se Også:

Clojure - Tekstbehandling: [https://clojure.org/guides/learn/strings](https://clojure.org/guides/learn/strings)

API-dokumentasjonen for `clojure.string`-pakken: [https://clojure.github.io/clojure/clojure.string-api.html](https://clojure.github.io/clojure/clojure.string-api.html).

Å lære Regulære Uttrykk: [https://learnbyexample.github.io/learn_regex/](https://learnbyexample.github.io/learn_regex/)

---