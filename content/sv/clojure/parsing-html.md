---
title:                "Tolka HTML"
date:                  2024-01-20T15:31:00.002513-07:00
html_title:           "Arduino: Tolka HTML"
simple_title:         "Tolka HTML"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/clojure/parsing-html.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Parsing HTML innebär att omvandla HTML-kod till en datastruktur som ditt program kan hantera. Programmerare gör detta för att extrahera data, manipulera innehållet eller skrapa webbsidor.

## Så här gör du:
Clojure har kraftfulla bibliotek för HTML-parsing, som `enlive` eller `hickory`. Här använder vi `hickory` för dess enkelhet:

```clojure
;; Lägg till hickory i dina projektberoenden first
;; [hickory "0.7.1"] for example (check for the latest version)

(require '[hickory.core :as hickory]
         '[clojure.java.io :as io])

;; Läs och parse HTML
(let [html-str (slurp (io/resource "example.html")) ; antar att "example.html" finns i resurser
      doc (hickory/parse html-str)]
  ;; Hitta alla länkar
  (hickory/select doc [:a]))
```
Sample output kan variera beroende på HTML-innehållet, men du kan förvänta dig något som:
```clojure
({:tag :a, :attrs {:href "http://exempel.com"}, :content ["Exempellänk"]})
```

## Deep Dive
HTML-parsing i Clojure går tillbaka till språkets Java-rötter, med många bibliotek bryggandes Java och Clojure. Alternativ som `enlive` använder djup selector-syntakt för att manipulera HTML, medan `hickory` bygger på den bekanta `hiccup`-syntaxen.

Implementationen av HTML-parsing i Clojure händer oftast så här: HTML läses in som en sträng, parsningsbiblioteket konverterar strängen till en Clojure-datastruktur (ofta ett träd), och sedan kan du navigera/traversera detta träd för att hitta, ändra eller extrahera delar av HTML-dokumentet.

## Se även:
- Hickory GitHub Repo: [https://github.com/davidsantiago/hickory](https://github.com/davidsantiago/hickory)
- Enlive GitHub Repo: [https://github.com/cgrand/enlive](https://github.com/cgrand/enlive)
- ClojureDocs, en community-drivna samling av Clojure-exempel och dokumentation: [https://clojuredocs.org/](https://clojuredocs.org/)
