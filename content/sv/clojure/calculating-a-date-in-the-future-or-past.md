---
title:    "Clojure: Beräkning av ett datum i framtiden eller det förflutna"
keywords: ["Clojure"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/sv/clojure/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Varför

Att kunna beräkna datum i framtiden eller förflutna är en mycket användbar funktion i många programmeringsprojekt. Genom att lära dig hur man gör det i Clojure kan du enkelt implementera denna funktion i dina egna program.

## Hur man gör det

För att kunna beräkna datum i framtiden eller förflutna behöver vi använda oss av vissa funktioner och datatyper i Clojure. Vi kommer att använda oss av den inbyggda funktionen `clojure.java.time` och datatypen `java.time.LocalDate`.

Först måste vi importera `clojure.java.time`-biblioteket:

```
(ns my-project.core
  (:require [clojure.java.time :as time]))
```

Sedan kan vi deklarera ett datum i framtiden eller förflutna genom att använda funktionen `time/local-date` och ge den ett antal argument som bestämmer vilket datum vi vill ha. Till exempel, för att få datumet 1 januari 2022, skulle vi skriva:

```
(time/local-date 2022 1 1)
```

För att få datumet 5 dagar framåt från det aktuella datumet, skulle vi skriva:

```
(let [now (time/local-date)]
  (time/plus now (* 5 time/DAY)))
```

Och för att få datumet 2 månader tillbaka från det aktuella datumet, skulle vi skriva:

```
(let [now (time/local-date)]
  (time/minus now (* 2 time/MONTH)))
```

## Djupdykning

Det finns även andra användbara funktioner och datatyper i `clojure.java.time`-biblioteket som kan vara värda att utforska. Till exempel kan vi använda funktionen `time/plus-years` eller `time/plus-months` för att direkt lägga till årtal eller månader till ett given datum, istället för att multiplicera antalet år eller månader med konstanterna `time/YEAR` eller `time/MONTH`.

Det är också viktigt att komma ihåg att `java.time.LocalDate`-datatypen inte bara representerar datum, utan också tider och tidszoner. Det betyder att du även kan använda funktioner som `time/plus-hours` eller `time/plus-minutes` för att justera tiden i ett datum.

## Se även

- Dokumentation för `clojure.java.time`-biblioteket: https://clojure.github.io/java.time-api/
- Bra reflektion på datatyper och funktioner för att hantera datum i Clojure: https://lispcast.com/datatype-and-function-mondays-clojure-dates/