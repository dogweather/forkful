---
title:                "Clojure: Beräkna ett datum i framtiden eller det förflutna"
simple_title:         "Beräkna ett datum i framtiden eller det förflutna"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/clojure/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

# Varför
Att kunna beräkna datum i framtiden eller förflutna är en viktig färdighet inom programmering. Det kan vara användbart för att planera uppgifter och händelser eller för att lösa problem som rör tidsövergångar.

# Hur man gör det

För att kunna beräkna datum i framtiden eller förflutna i Clojure, behöver du använda funktionen `clj-time` som finns tillgänglig som ett externt bibliotek.

```
(require '[clj-time.core :as time])
```

För att beräkna ett datum i framtiden, kan du använda funktionen `plus` tillsammans med en tidssträng och ett antal enheter (år, månader, dagar, etc.).

```
(time/plus (time/today) (time/years 2))
```

Det här exemplet lägger till två år till dagens datum. Du kan också använda funktionen `minus` för att beräkna datum i förflutna.

```
(time/minus (time/today) (time/days 10))
```

I dessa exempel används funktionen `today` för att få dagens datum som startpunkt.

# Djupdykning

För att kunna göra mer exakta beräkningar av datum, kan det vara användbart att förstå hur tidssträngar fungerar i Clojure. Fler exempel på tidsenheter som kan användas tillsammans med `plus` och `minus` är `months`, `weeks`, `hours` och `minutes`.

Det finns också andra funktioner i `clj-time` som kan vara användbara för att manipulera datum, såsom `before?`, `after?` och `duration`. Du kan läsa mer om dessa på bibliotekets GitHub-sida.

# Se även

- [Clj-Time Dokumentation] (https://github.com/clj-time/clj-time)
- [Clojure Officiell Hemsida] (https://clojure.org/)
- [Clojurians Slack Community] (https://clojurians.slack.com/)