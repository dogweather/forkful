---
title:                "Eine Webseite herunterladen"
html_title:           "Arduino: Eine Webseite herunterladen"
simple_title:         "Eine Webseite herunterladen"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/clojure/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Was & Warum?

Webseiten herunterladen ist der Prozess des Abrufens von Dateien von einem Server über das Internet. Programmierer tun dies meist, um den Inhalt der Seite für die Analyse oder Offline-Nutzung zu erfassen.

## Wie man:

Hier ist ein einfacher Clojure-Code, um eine Webseite herunterzuladen:

```Clojure
(ns demo.webdownload
  (:require [clojure.java.io :as io]))

(defn download-site
  [site-path]
  (let [site (io/input-stream site-path)
        webpage (slurp site)]
    (prn webpage)))

(download-site "https://www.example.com")

```
Die Ausgabe wird der Inhalt der aufgerufenen Webseite sein.

## Tiefere Einblicke

Webseiten-Herunterladen hat eine lange Geschichte, die bis zu den Anfängen des WWW zurückreicht. Es gibt verschiedene Alternativen und Methoden, die je nach Kontext, z.B. Ausführungsgeschwindigkeit oder Ressourcenverbrauch, besser sein können. Sie könnten `clj-http.client` für komplexere Anforderungen verwenden, aber die in diesem Artikel vorgestellte Methode ist für einfache Anforderungen ausreichend.

Die Implementierungsdetails des Webseiten-Download variiert je nach der spezifischen Programmiersprache und der Nutzung. In Clojure verwenden wir häufig `slurp` und `java.io.input-stream` zur Vereinfachung. 

## Mehr zu sehen

- Clojure offizielle Dokumentation: (https://clojure.org)
- Übersicht über das Herunterladen von Webseiten: (https://en.wikipedia.org/wiki/Web_scraping)
- clj-http.client Dokumentation:  (https://github.com/dakrone/clj-http)

 Bitte beachten Sie, dass Web-Scraping legalen und ethischen Bedenken unterliegt. Informieren Sie sich immer über diese Bedenken, bevor Sie eine Webseite scrapen.