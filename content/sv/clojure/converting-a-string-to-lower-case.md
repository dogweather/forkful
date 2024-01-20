---
title:                "Omvandla en sträng till gemener"
html_title:           "Arduino: Omvandla en sträng till gemener"
simple_title:         "Omvandla en sträng till gemener"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/clojure/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

---
title: "Konvertera sträng till små bokstäver i Clojure"
---

## Vad & Varför?

Att konvertera en sträng till små bokstäver innebär att omvandla alla stora bokstäver i en given sträng till motsvarande små bokstäver. Programmerare gör detta ofta för att jämföra strängar utan att behöva ta hänsyn till skillnader i stora och små bokstäver.

## Hur gör man:

Här är ett exempel på hur du konverterar en sträng till små bokstäver i Clojure:

```clojure
(defn str-to-lower [s]
  (.toLowerCase s))
  
(str-to-lower "Hej VÄRLDEN!")
```
När du kör ovanstående kod kommer utmatningen att vara:

```clojure
"hej världen!"
```

## Djupdykning

Clojures `.toLowerCase` funktion är en direkt åtkomst till Javas inbyggda metod för att konvertera textsträngar till små bokstäver, vilket är en standardmetod som finns tillgänglig för alla strängar i Java.

Originalet till denna metod går tillbaka till början av teckenformateringsstandarder, när behovet upstod att kunna jämföra och manipulera teckenbaserad information på ett enhetligt sätt.

Ett alternativ till `.toLowerCase` är att använda funktionen `clojure.string/lower-case`, vilket är den idiomatiske Clojure-sättet att konvertera en sträng till små bokstäver.

Här är ett exempel på hur du använder `clojure.string/lower-case` funktionen:

```clojure
(require '[clojure.string :as str])

(str/lower-case "Hej VÄRLDEN!")
```

Utmärkande för `str/lower-case` funktionen är att den returnerar en ny sträng där alla karaktärer har konverterats till små bokstäver, vilket innebär att ursprungssträngen inte påverkas.

## Se även

Om du vill ha ytterligare referenser och resurser om detta ämne, kolla in följande länkar:

* Javas `toLowerCase` metod i Oracle Docs: [https://docs.oracle.com/javase/8/docs/api/java/lang/String.html#toLowerCase--](https://docs.oracle.com/javase/8/docs/api/java/lang/String.html#toLowerCase--)