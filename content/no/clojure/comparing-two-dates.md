---
title:                "Clojure: Sammenligning av to datoer"
simple_title:         "Sammenligning av to datoer"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/clojure/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Hvorfor

Sammenligning av datoer er en vanlig oppgave ved programmering. Det kan være nyttig for å sortere data, filtrere ut informasjon eller for å lage komplekse logikker. Å kunne sammenligne to datoer i Clojure kan være nyttig for å gjøre koden din mer effektiv og nøyaktig.

## Hvordan du gjør det

For å sammenligne to datoer i Clojure, kan du bruke funksjonen `clojure.time/compare`. Denne funksjonen sammenligner to datoer og returnerer et tall som viser hvilken av datoene som er større. La oss se på et eksempel:

```clojure
(require '[clojure.time :as time])
(def today (time/today))
(def tomorrow (time/tomorrow))
(time/compare today tomorrow)
```

Dette vil gi følgende output:

```clojure
-1
```

Her indikerer tallet `-1` at `today` er mindre enn `tomorrow`. Du kan også sammenligne med andre operatører, for eksempel `>` eller `<`. La oss se på et annet eksempel:

```clojure
(> tomorrow today)
```

Dette vil gi følgende output:

```clojure
true
```

Som du kan se, er dette en enkel måte å sammenligne datoer på i Clojure.

## Dykk dypere

Når man sammenligner datoer, er det viktig å huske på at Clojure bruker Joda-time biblioteket for å håndtere datoer og tidspunkt. Dette betyr at det er mange funksjoner og metoder tilgjengelig for å gjøre mer komplekse sammenligninger. For å lære mer om Joda-time i Clojure, kan du sjekke ut offisiell dokumentasjon: https://clojure.github.io/java.time-api/introduction.html.

## Se også

- https://clojure.github.io/java.time-api/introduction.html
- https://github.com/clj-time/clj-time/wiki/Time-Comparisons