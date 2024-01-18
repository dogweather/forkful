---
title:                "Konvertere en dato fra en streng"
html_title:           "Clojure: Konvertere en dato fra en streng"
simple_title:         "Konvertere en dato fra en streng"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/clojure/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?

Å parse en dato fra en streng betyr å konvertere en tekstrepresentasjon av en dato til en datotype. Dette er viktig for programmerere fordi det gjør det mulig å håndtere og manipulere datoer i koden i stedet for bare å vise dem som tekst.

## Hvordan:

```Clojure
(require '[clj-time.coerce :as coerce])
(coerce/from-string "2020-10-01") ; => #inst"2020-10-01T00:00:00.000000000-00:00"
(coerce/to-date (coerce/from-string "2020-10-01")) ; => #date"2020-10-01"
```

En streng med dato kan også være mer kompleks, som å inkludere både dato og klokkeslett:

```Clojure
(coerce/from-string "2020-10-01T10:30:00") ; => #inst"2020-10-01T10:30:00.000000000-00:00"
(coerce/time-zone "Europe/Oslo" (coerce/from-string "2020-10-01T10:30:00")) ; => #inst"2020-10-01T08:30:00.000000000-00:00"
```

## Dypdykk:

Det å konvertere en dato fra en streng er en viktig del av å jobbe med datoer i programmering. Først og fremst gir det muligheten til å håndtere datoer i koden, noe som gjør koden mer dynamisk og funksjonell. Alternativer til å parse datoer fra strenger inkluderer å bruke biblioteker som Joda-Time og java.time i Java. I Clojure er det clj-time biblioteket som brukes til å håndtere datoer.

## Se også:

[clj-time dokumentasjon](https://github.com/clj-time/clj-time)