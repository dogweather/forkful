---
title:                "Kalkulere en dato i fremtiden eller fortiden"
html_title:           "Clojure: Kalkulere en dato i fremtiden eller fortiden"
simple_title:         "Kalkulere en dato i fremtiden eller fortiden"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/clojure/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

"## Hvorfor"

Hvorfor vil noen ønske å regne ut en dato i fremtiden eller fortiden? Det kan være nyttig for å planlegge fremtidige hendelser eller for å spore tidligere hendelser.

## Hvordan

Hvis du vil beregne en dato i fremtiden eller fortiden i Clojure, kan du bruke funksjonen `plus`, `minus` og `date-time` fra biblioteket `clojure.java-time`. Her er et eksempel på hvordan du kan bruke disse funksjonene:

```Clojure
(require '[java-time :as time])

(time/plus (time/date-time 2020 11 11)
           (time/duration (time/period 1 :years)
                          (time/period 2 :months)
                          (time/period 3 :days) ))
```

Dette vil gi oss en ny dato som er ett år, to måneder og tre dager etter 11. november 2020. Output vil se slik ut: `#object[java.time.LocalDateTime 0xe79f32a2 "2021-01-14T00:00"]`.

For å beregne en dato i fortiden, kan du bruke `time/minus` funksjonen og spesifisere negative perioder.

## Dykk dypere

Funksjonene `plus`, `minus` og `date-time` gjør det enkelt å beregne datoer i fremtiden eller fortiden i Clojure. Men det er også mulig å bruke andre funksjoner fra `clojure.java-time` biblioteket for å manipulere datoer.

Du kan for eksempel bruke `time/day-of-week` for å få ut hvilken ukedag en bestemt dato faller på, eller `time/format` for å formatere datoen på en spesifikk måte.

## Se også

- [`clojure.java-time` dokumentasjon](https://github.com/dm3/clojure.java-time)
- [Offisiell Clojure dokumentasjon](https://clojure.org/)
- [En guide til å lære Clojure](https://clojure.org/guides/learn/syntax)