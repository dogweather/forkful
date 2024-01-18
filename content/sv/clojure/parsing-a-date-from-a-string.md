---
title:                "Analysera ett datum från en sträng"
html_title:           "Clojure: Analysera ett datum från en sträng"
simple_title:         "Analysera ett datum från en sträng"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/clojure/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

# Vad & Varför?

Att parsa ett datum från en sträng innebär att omvandla en text till ett datum-objekt som datorn kan förstå. Detta är användbart för programmerare när de behöver hantera datum i sina applikationer, till exempel när man vill sortera eller filtrera data efter datum.

# Hur man:

```Clojure
(require '[clojure.java-time :as t])

;; Parsa datum från en sträng med standardformatet "yyyy-MM-dd"
(t/parse "2020-12-25") ;; => #object[java.time.LocalDate 0x440b4fcf "2020-12-25"]

;; Parsa datum från en sträng med ett anpassat format
(t/parse "12/25/2020" (t/formatter "MM/dd/yyyy")) ;; => #object[java.time.LocalDate 0x3b181f35 "2020-12-25"]
```

# Djupdykning:

Parsning av datum är en vanlig uppgift i programmering, och olika programmeringsspråk har olika inbyggda funktioner för detta. Alternativ till Clojure inkluderar Java's SimpleDateFormat och Ruby's Date.parse. Clojure har dock en stor fördel med sin Java-inbyggnad genom clojure.java-time-biblioteket som ger enkel och smidig hantering av datum och tid.

# Se även:

- [clojure.java-time dokumentation](https://clojure.java-time.io/)
- [Java SimpleDateFormat](https://docs.oracle.com/javase/7/docs/api/java/text/SimpleDateFormat.html)
- [Ruby Date.parse Dokumentation](https://ruby-doc.org/stdlib-2.5.1/libdoc/date/rdoc/Date.html#method-c-parse)