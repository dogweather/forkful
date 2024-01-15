---
title:                "Dekodning av html"
html_title:           "Clojure: Dekodning av html"
simple_title:         "Dekodning av html"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/clojure/parsing-html.md"
---

{{< edit_this_page >}}

## Varför
Du kanske undrar, varför skulle någon vilja lära sig att parsa HTML med Clojure? Svaret är enkelt: HTML-parsing är ett vanligt problem inom webbutveckling och med Clojure kan du enkelt hantera och behandla detta dataformat på ett effektivt sätt.

## Hur man gör det
För att parsa HTML med Clojure behöver du först importera biblioteket [enlive](https://github.com/cgrand/enlive/) som ger dig verktyg för att hantera och manipulera HTML-strukturer. När du väl har importerat biblioteket kan du använda funktionen `html-resource` för att hämta HTML från en URL:

```Clojure
(ns min-app.core
  (:require [net.cgrand.enlive-html :as html]))

(def html-tree (html/html-resource "https://example.com"))
```

Nu har du ett HTML-träd som du kan navigera i för att hitta och extrahera den information du behöver.

En bra övning är att försöka hämta ett visst element från HTML-dokumentet, till exempel en rubrik, och spara det som en sträng:

```Clojure
(def headline (html/select html-tree [:h1])) ; väljer alla h1-element
(def headline-str (html/text (first headline))) ; sparar den första som en sträng
```

Du kan också använda en kombination av selektorer och attribut för att välja specifika element, till exempel alla länkar med en viss klass:

```Clojure
(def links (html/select html-tree [:a.class "my-link"]))
```

## Djupdykning
En viktig del av att parsa HTML är att förstå hur selektorer fungerar. Enlive använder Clojures vektorer och map-funktioner för att välja och bearbeta element. Till exempel kan du använda `text` för att hämta texten från ett valt element, `attr` för att hämta ett attributvärde eller `html-snippet` för att hämta en del av HTML-koden.

Det finns också en mängd olika funktioner för att hantera och manipulera HTML-strukturer som kan vara användbara för mer avancerad parsing.

## Se även
Här är några resurser för att fördjupa din kunskap om att parsa HTML med Clojure:

- [Clojure for the Brave and True](https://www.braveclojure.com/clojure-for-the-brave-and-true/) - en utmärkt bok som täcker en mängd ämnen inom Clojure, inklusive HTML-parsing.
- [The Clojure Enthusiast](https://clojure.org/community/enthusiast) - en community av Clojure-entusiaster som delar kunskap och resurser.
- [Clojure for Data Science](https://clojure.org/science) - en samling av data science-tutorials på Clojure:s officiella hemsida.

Lycka till med din HTML-parsing med Clojure!