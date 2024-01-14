---
title:                "Clojure: Arbetar med json"
simple_title:         "Arbetar med json"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/clojure/working-with-json.md"
---

{{< edit_this_page >}}

## Varför

JSON är en vanligt använd dataformat i webbutveckling och samlar in data från andra system och API:er. Att lära sig hur man arbetar med JSON i Clojure kommer att öppna upp en hel värld av möjligheter för att skapa dynamiska och interaktiva webbapplikationer.

## Hur man gör

För att arbeta med JSON i Clojure behöver du först importera biblioteket för JSON-parsing. Detta kan göras genom att lägga till följande kod i din projekt.clj fil:

```Clojure
:dependencies [[org.clojure/data.json "1.0.0"]]
```

När biblioteket har laddats in kan du använda funktionen `read-str` för att läsa in en JSON-sträng och konvertera den till Clojure datastrukturer. Till exempel:

```Clojure
(require '[clojure.data.json :as json])

(def json-str "{\"name\":\"Johan\",\"age\":28}")
(def data (json/read-str json-str))
```

Om vi sedan skriver ut värdet av `data` ser vi att det är en Clojure-map med nycklarna `name` och `age` och deras motsvarande värden.

```Clojure
(user=> data)
{"name" "Johan", "age" 28}
```

För att skapa en JSON-sträng från Clojure datastrukturer kan vi använda funktionen `json/write-str`. Till exempel:

```Clojure
(def data {:name "Maria", :age 32})
(def json-str (json/write-str data))
```

Nu är `json-str` en textsträng som representerar vår data i JSON-format.

```Clojure
(user=> json-str)
"{\"name\":\"Maria\",\"age\":32}"
```

## Utforska djupare

JSON i Clojure är mycket mer än bara att läsa och skriva data. Det finns också funktioner för att konvertera datastrukturer mellan JSON och EDN (Extensible Data Notation), som är ett annat dataformat som används i Clojure. Dessutom finns det hjälpfunktioner för att manipulera och söka genom JSON-datastrukturer.

Om du vill lära dig mer om JSON i Clojure kan du läsa dokumentationen för biblioteket `clojure.data.json` eller utforska andra bibliotek som erbjuder mer avancerade funktioner för JSON-hantering.

## Se även

- [Clojure Data JSON biblioteket](https://github.com/clojure/data.json)
- [EDN dokumentation](https://github.com/edn-format/edn)
- [JSON formatterare](https://jsonformatter.curiousconcept.com/)