---
title:                "Tolka ett datum från en sträng"
date:                  2024-01-20T15:35:21.197878-07:00
html_title:           "Bash: Tolka ett datum från en sträng"
simple_title:         "Tolka ett datum från en sträng"

category:             "Clojure"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/clojure/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att tolka ett datum från en sträng innebär att konvertera texten till ett datumobjekt som ditt program kan förstå och använda. Programmerare gör detta för att kunna bearbeta och jämföra datum, ofta hämtade från olika datakällor.

## Hur man gör:
```Clojure
;; Lägg till clj-time biblioteket - en bra resurs för datumhantering
(require '[clj-time.format :as fmt])
(require '[clj-time.coerce :as coerce])

;; Fördefinierad formatterare
(def formatter (fmt/formatters :basic-date-time))

;; Parsing från sträng till datum
(def date-str "20230412T152045Z")
(def parsed-date (fmt/parse formatter date-str))

;; Visa det parsade datumet
(println parsed-date)

;; Omvandla tillbaka till sträng
(println (fmt/unparse formatter parsed-date))
```
Sample output:
```
#object[org.joda.time.DateTime 0x6d3c7871 "2023-04-12T15:20:45.000Z"]
"20230412T152045Z"
```

## Fördjupad information:
Historiskt har datumhantering i Clojure ofta handlat om att använda Java's Date klass direkt. Men `clj-time`, en omslutning av Joda-Time biblioteket, gjorde det lättare och mer idiomatiskt för Clojure. Alternativ finns såsom `java.time` i Java 8, vilket också kan användas direkt från Clojure. Detaljerna i implementeringen beror på bibbiblioteket du väljer, men principerna är desamma: översätta strängformat till något programmerbart.

## Se även:
- [clj-time GitHub-repositoriet](https://github.com/clj-time/clj-time)
- [Clojure's java-time bibliotek](https://github.com/dm3/clojure.java-time)
- [ClojureDocs, en gemenskapsdriven dokumentationssida](https://clojuredocs.org/)
