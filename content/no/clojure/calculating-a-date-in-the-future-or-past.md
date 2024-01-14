---
title:    "Clojure: Beregning av en dato i fremtiden eller fortiden"
keywords: ["Clojure"]
---

{{< edit_this_page >}}

## Hvorfor

Beregning av datoer i fremtiden eller fortiden kan være nyttig for å planlegge hendelser eller spore viktige datoer. Det kan også være nyttig når du jobber med tidsfølsomme data i programmering.

## Hvordan

For å beregne en dato i fremtiden eller fortiden i Clojure, kan du bruke funksjonen `clj-time.core/pplus` og `clj-time.core/pminus` som tar inn en dato og et antall enheter som skal legges til eller trekkes fra. Eksempelvis, hvis du ønsker å få 2 uker frem i tid fra i dag, kan du skrive:

```Clojure
(require '[clj-time.core :as time])

(def today (time/today))
(def two-weeks-from-now (time/pplus today (time/weeks 2)))

(println two-weeks-from-now) ; Output: #inst "2021-09-29T00:00:00.000-00:00"
```

I dette tilfellet brukte vi `time/weeks` for å indikere at vi ønsket å legge til 2 uker. Du kan også bruke andre tidsenheter som `days`, `hours` eller `minutes` avhengig av dine behov.

For å beregne en dato i fortiden, kan du bruke `pminus`-funksjonen og gi et negativt antall enheter som argument.

```Clojure
(def two-weeks-ago (time/pminus today (time/weeks 2)))

(println two-weeks-ago) ; Output: #inst "2021-09-15T00:00:00.000-00:00"
```

## Dykk dypere

For å få mer nøyaktige beregninger, kan du også bruke `pplus` og `pminus` med mindre enheter som `seconds` eller `milliseconds` for å få mer presise resultater. Clojure har også funksjoner som `time/in-minutes` eller `time/in-seconds` som kan hjelpe deg med å konvertere en dato og tiden til ønsket enhet.

## Se også

- [Clojure Cheat Sheet](https://clojure.org/api/cheatsheet)
- [Clojuredocs.org](https://clojuredocs.org/)
- [clj-time biblioteket](https://github.com/clj-time/clj-time)