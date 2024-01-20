---
title:                "Eine temporäre Datei erstellen"
html_title:           "Java: Eine temporäre Datei erstellen"
simple_title:         "Eine temporäre Datei erstellen"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/clojure/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Was & Warum?
Das Erstellen einer temporären Datei ist das Generieren einer kurzlebigen Datei für vorübergehende Speicherung oder Datenaustausch. Programmierer tun dies, um wertvollen Speicherplatz zu schonen und die Effizienz zu verbessern.

## Wie macht man das:
```Clojure
(require '[clojure.java.io :as io])

(defn create-temp-file 
  [prefix suffix]
  (io/file (io/make-temp-file prefix suffix)))

(def temp-file (create-temp-file "prefix" ".suffix"))

(print temp-file)
```
Die obigen Codeblöcke erstellen eine temporäre Datei und drucken ihren Pfad aus.

## Tiefgang:
Historisch gesehen wurden temporäre Dateien verwendet, um begrenzten Speicher zu managen. Heute sind sie immer noch nützlich, insbesondere für datenintensive Operationen. 

Alternativen beinhalten die Verwendung von In-Memory-Datenstrukturen wie Hash-Maps oder Vektoren. Allerdings sind sie nicht so effizient wie temporäre Dateien, wenn es um große Datenmengen geht.

In Clojure erstellt `io/make-temp-file` tatsächlich eine Instanz von `java.io.File`. Der resultierende temporäre Dateipfad ist systemabhängig, weil er auf dem von der Java-Systemeigenschaft `java.io.tmpdir` definierten temporären Verzeichnis basiert.

## Mehr Informationen:
- Clojure official documentation (Offizielle Clojure Dokumentation): [Clojure - Java Interop](https://clojure.org/reference/java_interop)
- StackOverflow (Stack Auslauft): [How to create a temporary directory/file in Clojure?](https://stackoverflow.com/questions/7517594/how-to-create-a-temporary-directory-file-in-clojure)