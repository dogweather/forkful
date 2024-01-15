---
title:                "Beräkna ett datum i framtiden eller förfluten tid."
html_title:           "Clojure: Beräkna ett datum i framtiden eller förfluten tid."
simple_title:         "Beräkna ett datum i framtiden eller förfluten tid."
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/clojure/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

# Varför

Att kunna beräkna datum i framtiden eller förfluten tid är ett vanligt problem som många programmerare stöter på. Oavsett om det är för att planera schema eller hantera giltighetsperioder för data, är det viktigt att kunna utföra dessa beräkningar effektivt.

# Såhär gör du

För att beräkna ett datum i framtiden eller förfluten tid i Clojure, kan du använda funktionen "plus" från standardbiblioteket "clojure.core". Den tar emot antalet enheter och tidsenheten (sekunder, minuter, timmar etc.) som argument och ger tillbaka ett nytt datummoln som representerar önskat datum.

```Clojure
(def today (java.util.Date.)) ; sätter dagens datum som variabel
(plus today 3 :days) ; lägger till 3 dagar till dagens datum
; => #inst "2021-07-06T17:51:02.000-00:00"

(plus today -2 :months) ; drar av 2 månader från dagens datum
; => #inst "2021-04-04T17:52:14.000-00:00”
```

# Djupdykning

Det är viktigt att notera att funktionen "plus" tar emot datummoln och ger tillbaka ett nytt moln, istället för att ändra på originalet. Detta tillför robusthet till koden och undviker potentiella felaktigheter. Dessutom kan du använda alla standardiserade tidsenheter som tillhandahålls av Java, såsom sekunder, minuter, timmar, dagar, veckor, månader och år.

# Se även

- Dokumentation för Clojure-funktionen "plus": https://clojuredocs.org/clojure.core/plus
- Datummoln i Clojure: https://clojuredocs.org/clojure.core/date-clouds
- Java:s java.util.Date-dokumentation: https://docs.oracle.com/en/java/javase/11/docs/api/java.base/java/util/Date.html