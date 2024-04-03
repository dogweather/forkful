---
date: 2024-01-20 17:36:12.878496-07:00
description: "Att omvandla ett datum till en str\xE4ng inneb\xE4r att du representerar\
  \ datumet i ett textformat. Programmerare g\xF6r detta f\xF6r att enklare kunna\
  \ visa, spara\u2026"
lastmod: '2024-03-13T22:44:37.536606-06:00'
model: gpt-4-1106-preview
summary: "Att omvandla ett datum till en str\xE4ng inneb\xE4r att du representerar\
  \ datumet i ett textformat."
title: "Omvandla ett datum till en str\xE4ng"
weight: 28
---

## Hur man gör:
För att konvertera datum till strängar i Clojure, använd `clj-time` biblioteket som bygger på Joda-Time. Detta exempel visar hur:

```Clojure
(require '[clj-time.format :as fmt])
(require '[clj-time.core :as t])

;; Skapar en formatter
(def formatter (fmt/formatters :basic-date-time))

;; Konverterar nuvarande tidpunkt till en sträng
(def now (t/now))
(def now-str (fmt/unparse formatter now))

println now-str ;; Exempel output: "20210405T121212.000Z"
```

## Fördjupning:
Tillbaka i tiden användes Java's `SimpleDateFormat` mycket, men den hade trådsäkerhetsproblem. Joda-Time, föregångaren till `java.time` paketet i Java 8, löste många av dessa problem och blev grundvalen för `clj-time`. Alternativt kan `java.time` användas direkt via Java interop eftersom Clojure kör på JVM. Implementeringsdetaljer involverar att välja rätt formatter för önskat datumsträngsformat och hantera tidszoner.

## Se även:
- clj-time GitHub repo: https://github.com/clj-time/clj-time
- Clojure doc för Java interop: https://clojure.org/reference/java_interop
- Joda-Time documentation: http://www.joda.org/joda-time/
- java.time documentation: https://docs.oracle.com/javase/8/docs/api/java/time/package-summary.html
