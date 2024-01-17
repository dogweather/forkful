---
title:                "Parsa html"
html_title:           "Clojure: Parsa html"
simple_title:         "Parsa html"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/clojure/parsing-html.md"
---

{{< edit_this_page >}}

### Vad & Varför?
Parsing HTML är processen där man omvandlar HTML-kod till ett mer läsbart format för att kunna manipulera och använda den på ett mer flexibelt sätt. Detta är användbart för programmerare som behöver extrahera data från webbsidor eller bearbeta deras innehåll.

### Hur man gör det:
```Clojure
(require '[clojure.data.xml :as xml])
(xml/parse-str "<h1>Hello, world!</h1>")
```
Detta kodexempel visar hur du kan använda Clojure's xml bibliotek för att konvertera en HTML taggsträng till en datastruktur som lätt kan bearbetas och navigeras igenom. Resultatet från ovanstående kod skulle vara en vektor som innehåller en map med attribut och ett barn som representerar innehållet av h1 taggen.

```Clojure
(xml/parse-str "<ul><li>Item 1</li><li>Item 2</li></ul>")
```
Detta exempel visar hur du kan hantera mer komplexa HTML-strukturer, som en lista av element. Resultatet skulle vara en vektor av vektorer, där varje inre vektor representerar innehållet av varje li tagg.

### Djupdykning:
Medan användandet av ett xml-bibliotek är det mest vanliga sättet att parsar HTML med Clojure, finns det alternativ som kan vara bättre lämpade för vissa användningsfall. Till exempel kan du använda biblioteket Enlive för att söka igenom och manipulera HTML-dokument med hjälp av CSS-selektorer. Du kan också använda ClojureScript för att parsar HTML i front-end applikationer.

Det finns också mer avancerade tekniker för parsing av HTML, som använder sig av regular expressions eller parsers som bygger på grammatikregler. Dessa kan vara användbara för specifika ändamål, men kan vara överkurs för de flesta användare.

### Se även:
- [Clojure Data Xml](https://github.com/clojure/data.xml) - det officiella xml biblioteket för Clojure.
- [Enlive](https://github.com/cgrand/enlive) - ett HTML manipulation bibliotek som använder CSS-selektorer. 
- [ClojureScript](https://clojurescript.org/) - ett språk som kompilerar till JavaScript, vilket gör det möjligt att använda Clojure för front-end utveckling.