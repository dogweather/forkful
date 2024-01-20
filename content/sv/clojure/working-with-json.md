---
title:                "Att arbeta med json"
html_title:           "Clojure: Att arbeta med json"
simple_title:         "Att arbeta med json"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/clojure/working-with-json.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att arbeta med JSON innebär att hantera data i ett format som är lätthanterligt och enkelt att läsa för både människor och datorer. JSON används ofta för att utbyta data mellan applikationer eller mellan en klient och en server. Det är också ett mycket populärt format för att lagra data i databaser. Programmerare använder JSON eftersom det är enkelt att förstå och använda, vilket gör det till ett utmärkt val för datahantering och utbyte.

## Så här:
 I Clojure finns en inbyggd funktion som heter `clojure.data.json` som möjliggör enkel hantering av JSON-data. Vi kan använda den för att konvertera data till JSON-format och vice versa. 

```Clojure
(require '[clojure.data.json :as json])
;; konvertera data till JSON
(json/write-str {:name "John" :age 30})
;; => "{\"name\":\"John\",\"age\":30}"

;; konvertera JSON till data
(json/read-str "{\"name\":\"John\",\"age\":30}")
;; => {:name "John", :age 30}
```

I det här exemplet använder vi `require` för att importera `clojure.data.json` och sedan använder vi `write-str` och `read-str` för att konvertera mellan data och JSON. Resultatet av `write-str` är en sträng, medan resultatet av `read-str` är en datastruktur i Clojure.

## Djupdykning:
JSON står för "JavaScript Object Notation" och har sitt ursprung från JavaScript-programmeringsspråket. Det är enkelt att förstå och skriva för människor, och är också lätt att läsa och skriva för datorer. Alternativ till JSON inkluderar XML, YAML och CSV.

I Clojure är `clojure.data.json` ett standardbibliotek som innehåller hjälpfunktioner för att arbeta med JSON. Det finns också andra bibliotek som tillhandahåller liknande funktioner, till exempel `cheshire` och `data.json`.

## Se även:
- [Officiell dokumentation för clojure.data.json](https://clojure.github.io/data.json/)
- [Jämförelse mellan olika dataformat](https://www.distilled.net/resources/json-vs-xml-a-performance-comparison/)