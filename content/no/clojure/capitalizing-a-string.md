---
title:                "Store bokstaver i en streng"
html_title:           "Clojure: Store bokstaver i en streng"
simple_title:         "Store bokstaver i en streng"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/clojure/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Hvorfor

Kapitalisering av strenger blir ofte brukt for å gjøre tekst mer leselig eller for å følge standard konvensjoner i programmering.

## Hvordan

For å kapitalisere en streng, kan du bruke funksjonen `clojure.string/capitalize` som tar inn en streng som argument. Her er et eksempel på hvordan du kan bruke denne funksjonen:

```Clojure
(clojure.string/capitalize "hello world") ; Output: "Hello world"
```

Som du kan se, blir den første bokstaven i strengen endret til stor bokstav. Dette gjelder uansett om den første bokstaven var en stor eller liten bokstav i utgangspunktet.

Hvis du ønsker å kapitalisere alle de første bokstavene i en setning, kan du bruke funksjonen `clojure.string/capitalize-words`:

```Clojure
(clojure.string/capitalize-words "hello world") ; Output: "Hello World"
```

Dette vil også gjelde for strenger som består av flere ord.

## Deep Dive

Det er viktig å merke seg at funksjonene `clojure.string/capitalize` og `clojure.string/capitalize-words` ikke bare bytter ut den første bokstaven i en streng. De bruker faktisk Unicode-standard regler for å kapitalisere strengen. Dette betyr at de håndterer spesielle tegn og diakritiske tegn korrekt.

Du kan også bruke funksjonen `clojure.string/upper-case` for å konvertere en hel streng til store bokstaver, og `clojure.string/lower-case` for å konvertere den til små bokstaver.

## Se også

- [Clojure strings dokumentasjon](https://clojure.org/api/cheatsheet)
- [Utbredte Unicode-standard regler for kapitalisering](https://www.unicode.org/charts/case/)