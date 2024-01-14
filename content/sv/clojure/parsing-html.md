---
title:                "Clojure: Analysera html"
simple_title:         "Analysera html"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/clojure/parsing-html.md"
---

{{< edit_this_page >}}

## Varför

HTML är det primära språket för att skapa webbsidor och det är därför viktigt för programmerare att kunna läsa och bearbeta HTML. Genom att lära sig att analysera och tolka HTML i Clojure kan du skapa mer dynamiska och flexibla webbapplikationer.

## Så här gör du

För att analysera HTML i Clojure finns det ett bibliotek som heter `enlive` som kan användas. Detta bibliotek ger enkla funktioner för att parsa HTML och extrahera information från det. Nedan följer ett exempel på hur man kan använda `enlive`:

```Clojure
(require '[net.cgrand.enlive-html :as html])

(def html-str "<html><body><h1>Hello World!</h1><p>This is a paragraph.</p></body></html>")

(html/at (html/html-resource (java.io.StringReader. html-str))
         [:h1])
```

I detta exempel använder vi `html/at` funktionen för att välja alla `<h1>` element i vår HTML-sträng. Detta kommer returnera en sekvens av dessa element som vi sedan kan bearbeta vidare.

## Djupdykning

Vid analys av HTML i Clojure är det viktigt att förstå strukturen och syntaxen i HTML. En viktig del är att kunna använda selektorer för att välja specifika delar av HTML-dokumentet. Selektorer är av stor hjälp när man arbetar med att extrahera information från en webbsida.

En annan viktig aspekt är att förstå hur man kan manipulera HTML-dokumentet genom att lägga till, ändra eller ta bort element och attribut. `enlive` biblioteket har funktioner för detta ändamål och genom att använda Clojures efterfrågade datastrukturer som vektorer och maps kan vi enkelt hantera HTML-kod.

## Se även

* [Officiell `enlive` dokumentation](https://github.com/cgrand/enlive)
* [Tutorials på svenska för att lära sig Clojure](https://github.com/svensk-programmering/lar-dig-clojure)
* [En guide för att lära sig HTML](https://www.w3schools.com/html/)