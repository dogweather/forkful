---
title:                "HTML parsen"
html_title:           "Arduino: HTML parsen"
simple_title:         "HTML parsen"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/clojure/parsing-html.md"
---

{{< edit_this_page >}}

# HTML Verarbeitung mit Clojure

## Was & Warum?

HTML-Verarbeitung ist die Methode zur Interpretation und Manipulation von HTML-Strukturen. Die Programmierer setzen sie ein, um strukturierte Daten aus Webseiten zu gewinnen oder diese zu manipulieren.

## Wie man es macht:

```Clojure
;;; Einbindung der Anforderungen
(require '[clj-http.client :as client])
(require '[hickory.core :as hickory])
(require '[hickory.select :as select])

;;; Webseitenabruf
(def res (client/get "https://example.com"))

;;; Parsen des HTML
(def doc (hickory/parse (:body res)))

;;; Auswahl eines spezifischen HTML-Elements
(defn get-elements [doc css-selector]
  (select/select (select/css css-selector) doc))
```
Dieses Beispiel zeigt wie man eine Webseite mit `clj-http.client` abruft und anschließend das HTML mit `hickory` parst. Mit der `get-elements` Funktion kann man spezifische HTML-Elemente auswählen.

## Vertiefung

HTML-Verarbeitung hat eine lange Geschichte, die mit der Entwicklung von HTML einhergeht. Es existieren Alternativen wie zum Beispiel `Jsoup`. `Jsoup` ist jedoch eine in Java geschriebene Bibliothek, also nicht nativ in Clojure.

Die `hickory` Bibliothek bietet eine schöne Schnittstelle zum Parsen und Auswählen von HTML-Elementen in Clojure. Es verwendet unter der Haube `jsoup` zum Parsen des HTML und erweitert es um eine clojurische API. Der Datenfluss in `hickory` wird durch Map-Transformationen dargestellt.

## Siehe Auch

- [Clojure's clj-http Bibliothek](https://github.com/dakrone/clj-http)
- [Hickory](https://github.com/davidsantiago/hickory)
- [Hickory's select Modul](https://github.com/davidsantiago/hickory/blob/master/src/hickory/select.clj)