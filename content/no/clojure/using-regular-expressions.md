---
title:    "Clojure: Å bruke regulære uttrykk"
keywords: ["Clojure"]
---

{{< edit_this_page >}}

## Hvorfor

Regular expressions, også kjent som "regex", er et kraftig verktøy i programmering for å søke og manipulere tekststrenger på en effektiv måte. Med regex kan du finne og erstatte deler av en tekst ved hjelp av spesielle uttrykk og symboler. Dette gjør regex veldig nyttig for oppgaver som datauttrekk, tekstanalyse, og formatering av data. 

## Hvordan

For å bruke regex i Clojure, må du først importere "clojure.string" biblioteket. Deretter kan du bruke funksjonen "re-find" eller "re-seq" for å søke etter mønstre i en streng. Her er et enkelt eksempel på hvordan regex kan brukes:

```Clojure
(require '[clojure.string :as str])

(def text "Heisann, dette er en tekststreng.")
(str/re-find #"tekst" text) ; returnerer "tekst"
```

Du kan også bruke regex for å erstatte deler av en tekst med "re-replace" funksjonen:

```Clojure
(str/re-replace #"tekst" "ord" text) ; returnerer "Heisann, dette er en ordstreng."
```

## Dypdykk

Regex kan være komplekst og tidkrevende å lære, men det finnes mange ressurser tilgjengelig for å hjelpe deg å forstå konseptet bedre. Her er noen tips for å komme i gang:

- Bruk en regex tester online for å eksperimentere med ulike uttrykk og se hvordan de fungerer.
- Lær de vanligste regex symbolene og deres betydning.
- Prøv å løse ulike utfordringer og problemer ved hjelp av regex istedenfor manuelt arbeid. Dette vil hjelpe deg å forstå konseptet bedre og øke din regex-fobi. 

## Se også

- [offisiell Clojure regex dokumentasjon] (https://clojure.github.io/clojure/clojure.string-api.html#clojure.string/re-find)
- [regex tester] (https://regex101.com/)
- [regex tutorial på norsk] (https://www.regexbuddy.com/regex.html?type=norsk)