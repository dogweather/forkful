---
title:                "HTML parsen"
date:                  2024-01-20T15:30:38.307168-07:00
html_title:           "Arduino: HTML parsen"
simple_title:         "HTML parsen"

category:             "Clojure"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/clojure/parsing-html.md"
---

{{< edit_this_page >}}

## What & Why? (Was & Warum?)
HTML-Parser lesen und interpretieren den HTML-Code einer Webseite. Programmierer nutzen das, um Daten zu extrahieren, automatisiert zu interagieren oder Inhalte zu prüfen.

## How to: (Wie geht das?)
Clojure bietet verschiedene Bibliotheken, um HTML zu parsen. `Enlive` ist eine beliebte Wahl. Hier ist ein schnelles Beispiel:

```Clojure
(ns mein-projekt.core
  (:require [net.cgrand.enlive-html :as html]))

(defn parse-html [html-str]
  (html/html-resource (java.io.StringReader. html-str)))

;; Nutzung:
(let [html-str "<html><body><p>Hello, Clojure!</p></body></html>"]
  (println (parse-html html-str)))
```

Ausgabe:

```
({:tag :html, 
  :attrs nil, 
  :content [{:tag :body, 
             :attrs nil, 
             :content [{:tag :p, 
                        :attrs nil, 
                        :content ["Hello, Clojure!"]}]}]})
```

Das Ergebnis ist eine verschachtelte Datenstruktur, die die HTML-Elemente abbildet.

## Deep Dive (Tiefergehendes)
Früher setzten Entwickler oft auf regelbasierte Ansätze mit regulären Ausdrücken, um HTML zu parsen. Aber HTML ist komplex und reguläre Ausdrücke sind fehleranfällig. Moderne Parser wie `Enlive` erzeugen eine Manipulierbare Baumstruktur.

`jsoup` ist eine Alternative, die Java-Bibliothek ist aber in Clojure weniger idiomatisch. `Hickory` wandelt HTML in Clojure-Datenstrukturen um, die direkte Manipulation in Clojure erlauben.

Enlive nutzt Selector-basierte Templates, um nicht nur zu parsen, sondern auch, um HTML zu transformieren - mächtig für Web Scraping und Templating.

## See Also (Siehe auch)
- Enlive-Dokumentation: [https://github.com/cgrand/enlive](https://github.com/cgrand/enlive)
- Hickory auf GitHub: [https://github.com/davidsantiago/hickory](https://github.com/davidsantiago/hickory)
- Jsoup: [https://jsoup.org/](https://jsoup.org/)
