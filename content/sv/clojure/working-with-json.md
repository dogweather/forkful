---
title:                "Arbeta med json"
html_title:           "Clojure: Arbeta med json"
simple_title:         "Arbeta med json"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/clojure/working-with-json.md"
---

{{< edit_this_page >}}

## Varför

JSON (JavaScript Object Notation) är en vanlig format för datautbyte och används ofta i webbutveckling och api design. Att lära sig hur man arbetar med JSON i Clojure kan hjälpa dig att bygga robusta och skalbara applikationer som kommunicerar med andra system.

## Hur man gör

Det finns flera sätt att arbeta med JSON i Clojure, men vi kommer att fokusera på de två mest vanliga metoderna: använda inbyggda funktioner eller använda ett bibliotek som heter Cheshire.

Först och främst, om du arbetar med små och enkla JSON-strängar kan du använda Clojure:s inbyggda ```read-string``` funktion för att konvertera JSON till Clojure datastrukturer:

```Clojure 
(def my-json-str "{\"name\":\"Kalle\",\"age\":31,\"hobbies\":[\"coding\",\"reading\"]}")

(read-string my-json-str)

;; Output
;; {:name "Kalle", :age 31, :hobbies ["coding" "reading"]}
```

Om du arbetar med större och mer komplexa JSON-strängar kan det vara bättre att använda en JSON-bibliotek som Cheshire. För att använda detta bibliotek behöver du först lägga till den som en beroende i din ```project.clj``` fil:

```Clojure
[org.clojure/data.json "1.0.2"]
```

Sedan kan du använda ```parse``` -metoden för att konvertera JSON till Clojure datastrukturer:

```Clojure
(require '[cheshire.core :as json])

(def my-json-str "{\"name\":\"Kalle\",\"age\":31,\"hobbies\":[\"coding\",\"reading\"]}")

(json/parse-string my-json-str)
 
;; Output
;; {:name "Kalle", :age 31, :hobbies ["coding" "reading"]}
```

Om du vill konvertera Clojure datastrukturer till JSON kan du använda ```generate-string``` -metoden:

```Clojure
(def my-clojure-map {:name "Kalle", :age 31, :hobbies ["coding" "reading"]})

(json/generate-string my-clojure-map)

;; Output
;; "{\"name\":\"Kalle\",\"age\":31,\"hobbies\":[\"coding\",\"reading\"]}"
```

## Djupdykning

För mer komplexa operationer som att hämta och hantera data från JSON API:er, kan det vara värt att titta djupare på Cheshire-biblioteket. Det finns många funktioner som underlättar arbetet med JSON, såsom ```select-keys``` för att välja specifika nycklar från en JSON-sträng och ```diff``` för att jämföra två JSON-strängar.

Det är också viktigt att ha i åtanke att vissa operationer på Clojure datastrukturer, som att lägga till eller ta bort nycklar, inte nödvändigtvis påverkar JSON-strängen. I dessa fall måste du använda ```generate-string``` -metoden för att konvertera tillbaka till JSON-strängen efter att ha utfört operationerna.

## Se även

- [Clojure officiella dokumentation om JSON](https://clojure.org/reference/data_structures#_json)
- [Cheshire dokumentation](https://github.com/dakrone/cheshire)