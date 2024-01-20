---
title:                "Omvandla ett datum till en sträng"
html_title:           "C#: Omvandla ett datum till en sträng"
simple_title:         "Omvandla ett datum till en sträng"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/clojure/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Vad & Varför? 

Att omvandla ett datum till en sträng (eller "string") är processen att omvandla objektet datum till en textuell representation. Detta görs så att datum kan visas för människor eller användas i beräkningar som kräver textformat.

## Hur man gör:

I Clojure kan du omvandla ett datum till en sträng med hjälp av `clj-time` biblioteket och `local-date` funktion. Låt oss se hur det fungerar:

```clojure
(ns my-app.core
    (:require [clj-time.format :as f]))

(def date-object (f/local-date-time))
(def date-string (f/unparse f/formatters :iso-date-time date-object))

(println date-string)
```

Det här programmet kommer att skriva ut det nuvarande datumet och tiden enligt ISO-8601-standard i form av en sträng.

## Djupdykning:

Omvandlingen av datum till strängar har varit en del av programmeringsspråk sedan de första high-level språken. Alternativet till att använda `clj-time` skulle vara att använda java interop och det inbyggda java.Date biblioteket, men `clj-time` biblioteket skapas för att vara mer idiomatic Clojure.

När du använder clj-time, blir detaljerna om hur omvandlingen görs hanteras av biblioteket, men i grunden använder det Java SimpleDateFormat-klassen för att omvandla datumet till en sträng.

## Se också:

För mer information, kolla in följande resurser:

1. [clj-time GitHub Repo](https://github.com/clj-time/clj-time) - källkoden och dokumentationen för `clj-time` biblioteket.

2. [Java SimpleDateFormat Dokumentation](https://docs.oracle.com/javase/8/docs/api/java/text/SimpleDateFormat.html) - detaljer om hur Java omvandlar datum till strängar.

3. [Clojure for the Brave and True](https://www.braveclojure.com) - en super bok för att lära sig mer om Clojure.