---
title:                "Suchen und Ersetzen von Text"
html_title:           "Bash: Suchen und Ersetzen von Text"
simple_title:         "Suchen und Ersetzen von Text"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/clojure/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

# Einfache Suche und Ersetzung in Clojure

## Was & Warum?

Die Suche und Ersetzung von Text ist eine gängige Operation in der Programmierung - sie hilft uns, Muster zu finden und durch andere zu ersetzen. Diese alltägliche Notwendigkeit steht im Zentrum vieler Aufgaben, vom Refactoring bis hin zur Datenbereinigung.

## Wie geht das:

Clojure macht diese Aufgabe einfach mit der Funktion `clojure.string/replace`. Hier sind einige Beispiele:

```Clojure
(require '[clojure.string :as str])

;; Ein Wort ersetzen
(str/replace "Hallo Welt!" "Welt" "Clojure")
;; => "Hallo Clojure!"

;; Mit einem Regulären Ausdruck ersetzen
(str/replace "abc 123 def" #"\d+" "456")
;; => "abc 456 def"
```

Die `str/replace` Funktion kann sowohl einen String als auch einen regulären Ausdruck als Suchparameter verwenden.

## Vertiefung:

Die Hilfsbibliothek von Clojure, `clojure.string`, bietet uns diese Funktionen und noch mehr. Historisch gesehen ist die Unterstützung für reguläre Ausdrücke eine tief verwurzelte Funktion in der UNIX-Welt und somit auch in der JVM-Welt, auf der Clojure aufbaut.

Es gibt viele Wege, um zur Lösung zu kommen. Eine Alternative zu `clojure.string/replace` ist es, die Sequenz von Zeichen selbst zu durchlaufen und die gewünschten Änderungen vorzunehmen. Das ist aber bei weitem komplizierter und anfälliger für Fehler.

Die Implementierung dieser Such- und Ersetzungsfunktion ist hervorragend optimiert und nutzt die mächtigen regulären Ausdrucksfunktionen der JVM. Das bedeutet, dass selbst komplexe und umfangreiche Such- und Ersetzungsoperationen effizient abgewickelt werden können.

## Siehe auch:

Für mehr Informationen über die `clojure.string` Bibliothek könnt ihr die [offizielle Dokumentation](https://clojure.github.io/clojure/clojure.string-api.html) besuchen. Es gibt auch einen guten Überblick über [reguläre Ausdrücke in Clojure](https://www.baeldung.com/clojure-regular-expressions) bei Baeldung.