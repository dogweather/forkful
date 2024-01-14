---
title:                "Clojure: Eine Webseite herunterladen."
simple_title:         "Eine Webseite herunterladen."
programming_language: "Clojure"
category:             "Clojure"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/clojure/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Warum

Das Herunterladen von Webseiten ist ein entscheidender Schritt beim Erstellen von Web Crawlers, Daten-Scraping oder einfach nur zum Speichern interessanter Inhalte. In dieser Anleitung werden wir untersuchen, wie man mithilfe von Clojure Webseiten herunterladen kann.

## Anleitung

```Clojure
(ns website-downloader.core
  (:require [clj-http.client :as client]))

(defn get-page [url]
  (let [response (client/get url)]
    (println "Status code:" (:status response))
    (:body response)))
```

Um eine Webseite herunterzuladen, verwenden wir die Bibliothek `clj-http` und die Funktion `get-page`. Sie nimmt die URL der Webseite als Argument und gibt den Inhalt der Seite als String zurück. Wir können auch den Statuscode der Antwort ausgeben und diesen für weitere Verarbeitungsschritte verwenden.

```Clojure
(def url "https://example.com")
(def page (get-page url))

(println page)
```

Im obigen Beispiel rufen wir die Funktion `get-page` auf und speichern den Rückgabewert in der Variable `page`. Anschließend geben wir den Inhalt der Webseite mittels `println` aus.

## Tiefer Tauchgang

Um effizienter mit heruntergeladenen Webseiten umgehen zu können, bietet Clojure auch die Möglichkeit, den Inhalt als Datenstruktur zu parsen.

```Clojure
(ns website-downloader.core
  (:require [clj-http.client :as client]
            [clojure.xml :as xml]))

(defn get-xml-page [url]
  (let [response (client/get url)
        xml-tree (xml/parse (:body response))]
    xml-tree))
```

In diesem Beispiel verwenden wir die Funktion `xml/parse`, um den Inhalt der Seite in eine Baumstruktur zu konvertieren. Anschließend können wir mit dem Datenzugriffsoperator `->` auf die einzelnen Elemente zugreifen.

```Clojure
(def url "https://example.com")
(def page-xml (get-xml-page url))

(-> page-xml
    :html
    :body
    :div
    :h1
    :content)
```

Wir können beispielsweise den Inhalt des `<h1>`-Tags mit Hilfe der oben gezeigten Syntax extrahieren und ausgeben lassen.

## Siehe auch

- [Offizielle Clojure-Dokumentation](https://clojure.org/)
- [clj-http Dokumentation](https://github.com/dakrone/clj-http)