---
title:                "Clojure: Å beregne en dato i fremtiden eller fortiden"
simple_title:         "Å beregne en dato i fremtiden eller fortiden"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/clojure/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

\# Hvorfor

Noen ganger er det nødvendig å beregne en dato i fremtiden eller fortiden for å løse programmeringsoppgaver. Dette kan være ved å finne ut når et produkt vil bli levert, når et abonnement utløper, eller når et arrangement vil finne sted. Clojure har flere funksjoner som gjør det enkelt å håndtere datoutregninger.

\# How To

For å beregne en dato i fremtiden eller fortiden i Clojure, kan du bruke funksjonen `clj-time.core/plus`. Den tar to argumenter: en dato og en duration. Datoen kan være en `java.util.Date`, `clj-time.core/now` (dagens dato), eller `clj-time.core/today` (dagens dato uten klokkeslett). Duration kan være et `clj-time.core/period`-objekt, som består av et antall år, måneder, uker, dager, timer, minutter og sekunder. Du kan også bruke bare et av disse feltene hvis du ikke trenger hele tidsperioden. For eksempel, hvis du vil beregne datoen to uker fra nå bruker du følgende kode:

```Clojure
(ns my-project.example
  (:require [clj-time.core :as time]))

(time/plus (clj-time.core/today) (clj-time.core/period :weeks 2)) ;; => #inst "2020-05-28T00:00:00.000+00:00"
```

Hvis du vil ha ut datoen uten tidsstempel, kan du bruke funksjonen `clj-time.core/date-parts`:

```Clojure
(time/date-parts (clj-time.core/plus (clj-time.core/now) (clj-time.core/period :years 1))) ;; => [2021 5 14]
```

\# Deep Dive

Funksjonen `clj-time.core/plus` støtter også "relative durations", som betyr at du kan bruke ord som "tomorrow" eller "next month" istedenfor å spesifisere en bestemt dato. Dette gjør det lettere å håndtere dynamiske beregninger. For eksempel, hvis du er på utkikk etter å beregne datoen for første søndag i neste måned kan du bruke følgende kode:

```Clojure
(time/plus (clj-time.core/date-parts (clj-time.core/now))
           (clj-time.core/period :months 1 :string "first :sunday"))
;; => #inst "2020-06-07T00:00:00.000+00:00"
```

\# Se Også

- [clj-time dokumentasjon](https://github.com/clj-time/clj-time "clj-time dokumentasjon")
- [Clojure.org](https://clojure.org/ "Clojure.org")
- [Clojure Programmering: bli kjent med Clojure](https://clojure.org/guides/getting_started "Clojure Programmering: bli kjent med Clojure")