---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:11:43.217252-07:00
description: "Wie geht das: Clojure verf\xFCgt nicht \xFCber eingebaute F\xE4higkeiten\
  \ zum Parsen von HTML, aber Sie k\xF6nnen Java-Bibliotheken oder Clojure-Wrapper\
  \ wie `enlive`\u2026"
lastmod: '2024-03-13T22:44:53.417506-06:00'
model: gpt-4-0125-preview
summary: "Clojure verf\xFCgt nicht \xFCber eingebaute F\xE4higkeiten zum Parsen von\
  \ HTML, aber Sie k\xF6nnen Java-Bibliotheken oder Clojure-Wrapper wie `enlive` oder\
  \ `hickory` nutzen."
title: HTML parsen
weight: 43
---

## Wie geht das:
Clojure verfügt nicht über eingebaute Fähigkeiten zum Parsen von HTML, aber Sie können Java-Bibliotheken oder Clojure-Wrapper wie `enlive` oder `hickory` nutzen. Hier ist, wie man beide verwendet:

### Verwendung von Enlive:
Enlive ist eine beliebte Wahl für das Parsen von HTML und Web Scraping. Fügen Sie es zunächst zu Ihren Projektabhängigkeiten hinzu:

```clojure
[net.cgrand/enlive "1.1.6"]
```

Dann können Sie HTML so parsen und navigieren:

```clojure
(require '[net.cgrand.enlive-html :as html])

(let [doc (html/html-resource (java.net.URL. "http://example.com"))]
  (html/select doc [:div.some-class]))
```

Dieser Schnipsel holt eine HTML-Seite und wählt alle `<div>` Elemente mit der Klasse `some-class` aus.

Die Ausgabe könnte folgendermaßen aussehen:

```clojure
({:tag :div, :attrs {:class "some-class"}, :content ["Hier ist etwas Inhalt."]})
```

### Verwendung von Hickory:
Hickory bietet eine Möglichkeit, HTML in ein Format zu parsen, das in Clojure einfacher zu bearbeiten ist. Fügen Sie Hickory Ihren Projektabhängigkeiten hinzu:

```clojure
[hickory "0.7.1"]
```

Hier ist ein einfaches Beispiel:

```clojure
(require '[hickory.core :as hickory]
         '[hickory.select :as select])

;; Parsen des HTML in das Hickory-Format
(let [doc (hickory/parse "<html><body><div id='main'>Hallo, Welt!</div></body></html>")]
  ;; Wählen Sie das Div mit der ID 'main'
  (select/select (select/id "main") doc))
```

Dieser Code parst einen einfachen HTML-String und verwendet einen CSS-Selektor, um ein `div` mit der ID `main` zu finden.

Beispielausgabe:

```clojure
[{:type :element, :tag :div, :attrs {:id "main"}, :content ["Hallo, Welt!"]}]
```

Sowohl `enlive` als auch `hickory` bieten robuste Lösungen für das Parsen von HTML in Clojure, wobei sich `enlive` mehr auf das Templating konzentriert und `hickory` die Datentransformation betont.
