---
title:                "Clojure: Beräkna ett datum i framtiden eller i det förflutna"
programming_language: "Clojure"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/clojure/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Varför
Det finns många anledningar till att vilja räkna ut ett datum i framtiden eller förfluten tid. Kanske vill du planera ett evenemang eller beräkna när en uppgift ska vara färdig. Ett annat scenario kan vara att du behöver veta hur länge en produkt har varit i lager eller när en kampanj ska avslutas.

## Hur man gör det
För att räkna ut ett datum i framtiden eller förfluten tid kan du använda dig av funktionen `java.util.Date`. Nedan följer ett kodexempel för att räkna ut ett datum fem dagar framåt och ett annat exempel för att räkna ut ett datum två månader bakåt.

```Clojure
(ns date-calculator.core
  (:require [clojure.java-time :as jt]))

(defn calculate-date [days]
  (let [now (jt/now)
        future-date (jt/plus now (jt/days days))]
    (.toString future-date)))

(defn calculate-prev-date [months]
  (let [now (jt/now)
        past-date (jt/minus now (jt/months months))]
    (.toString past-date)))

(println "Datumet 5 dagar framåt: " (calculate-date 5))
(println "Datumet 2 månader bakåt: " (calculate-prev-date 2))
```

Detta kommer att ge följande utmatning:

```
Datumet 5 dagar framåt: Fri May 22 23:19:38 CEST 2020
Datumet 2 månader bakåt: Mon Feb 24 23:19:38 CET 2020
```

## Djupdykning
För att förstå hur denna funktion fungerar behöver vi förstå hur datumen representeras i Clojure. I Clojure representeras datum som en sekvens av bakåt- eller framåtskiftande sekunder från 1 januari 1970 klockan 00:00:00 GMT. Dessa sekunder kallas för "Unix epoch", och det är denna representation som används av funktionen `java.util.Date`.

För att räkna ut ett datum i framtiden använder vi oss av funktionen `plus`, som tar två argument: ett datum och ett antal enheter (i vårt fall dagar). Funktionen `minus` fungerar på samma sätt, men drar istället bort enheter från datumen. För att få ut ett läsbart datum efter dessa beräkningar använder vi funktionen `toString`, som omvandlar datumen till en textsträng.

## Se även
- [Java Date and Time API](https://docs.oracle.com/javase/8/docs/api/java/util/Date.html)
- [Clojure Time Library](https://github.com/clj-time/clj-time)
- [Java.util.date i Clojure](https://docs.cider.mx/clojure/manipulating_dates.html)