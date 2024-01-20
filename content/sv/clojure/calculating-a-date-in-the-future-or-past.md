---
title:                "Beräkna ett datum i framtiden eller förflutna"
html_title:           "Clojure: Beräkna ett datum i framtiden eller förflutna"
simple_title:         "Beräkna ett datum i framtiden eller förflutna"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/clojure/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Vad & Varför?

Att beräkna ett datum i framtiden eller förflutna är att bestämma ett specifikt datum baserat på ett annat datum och tidsspann (dagar, veckor, månader, etc.). Programutvecklare gör detta för olika applikationer, t.ex. påminnelser, avbetalningar eller att spåra händelser över tid.

## Hur man gör:

För att beräkna framtida eller tidigare datum kan vi använda `clj-time` biblioteket i Clojure och dess `plus` och `minus` funktioner. 

``` Clojure 
(require '[clj-time.core :as t]
         '[clj-time.coerce :as c]
         '[clj-time.periodic :as p])

(def current-date (t/now))

(def future-date (t/plus current-date (t/days 7)))

(def past-date (t/minus current-date (t/days 7)))
``` 

Det ovanstående kodblocket visar hur vi kan beräkna datum en vecka i framtiden och dåtid från nuvarande datum.

## Djupdykning:

Historiskt har datumhanteringsbibliotek som `clj-time` sin grund i Joda-Time, ett breda använda Java-bibliotek. `clj-time` innehåller den grundläggande funktionaliteten i Joda-Time och lägger till en del clojure-specifika funktioner.

Om vi går in på alternativ, `java.time` biblioteket som tillhandahålls av Java 8 och senare är ett annat bra alternativ. Dock ger `clj-time` en mer idiomatisk Clojure-api ovanpå Joda-Time vilket gör det mer lockande för Clojure-utvecklare.

När det gäller genomförandedetaljer, `t/plus` och `t/minus` funktionerna tar ett datum och en period (t.ex. dagar, veckor) som argument och returnerar ett nytt datumobjekt. Det är viktigt att notera att originaldatumobjektet inte ändras här. Istället returneras en ny instans.

## Se också:

Här är några länkar till relaterade källor för mer information och lärande:

1. `clj-time` GitHub repo: [https://github.com/clj-time/clj-time](https://github.com/clj-time/clj-time).
   
2. Officiell dokumentation för `java.time` biblioteket: [https://docs.oracle.com/javase/8/docs/api/java/time/package-summary.html](https://docs.oracle.com/javase/8/docs/api/java/time/package-summary.html).

3. Joda-Time, den ursprungliga inspirationen för `clj-time`: [http://www.joda.org/joda-time/](http://www.joda.org/joda-time/).