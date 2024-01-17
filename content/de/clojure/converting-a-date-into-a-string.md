---
title:                "Umwandlung eines Datums in einen String"
html_title:           "Clojure: Umwandlung eines Datums in einen String"
simple_title:         "Umwandlung eines Datums in einen String"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/clojure/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Was & Warum?
Converting von einer Datum in einen String bedeutet, das Datum in einem lesbaren Textformat zu präsentieren. Programmierer tun dies, um Daten in einem für Menschen leichter interpretierbaren Format zu erhalten.

## Wie geht's:
- Die gängigste Methode, ein Datum in einen String umzuwandeln, ist die Funktion `str`, die ein Datum in einen String im Format `YYYY-MM-DD` konvertiert.
- Eine andere Möglichkeit ist die Verwendung der `format`-Funktion, die es ermöglicht, das Datum in einem benutzerdefinierten Format wie `DD.MM.YYYY` zu formatieren.
```Clojure 
;; Verwendung von 'str'
(str 2021 10 25)
;; Output: "2021-10-25"

;; Verwendung von 'format'
(format "Heute ist der %{dd}.%{MM}.%{YYYY}" 25 10 2021)
;; Output: "Heute ist der 25.10.2021"
```

## Tiefer tauchen:
- Die Umwandlung von Datum in einen String war in früheren Programmiersprachen eine komplizierte Aufgabe, da sie manuelle Berechnungen erforderte. Mit der Einführung von Funktionen wie `str` und `format` in Clojure ist dies jedoch ein einfacher Prozess geworden.
- Eine alternative Methode ist die Verwendung von Bibliotheken wie `clj-time` oder `chrono`, die eine Vielzahl von Funktionen zum Formatieren von Datum und Zeit bieten.
- Die `str`-Funktion verwendet tatsächlich die `toString`-Methode der Klasse `java.util.Date` für die Umwandlung von Datum in String.

## Sieh auch:
- Dokumentation zu den `str` und `format` Funktion von Clojure: https://clojuredocs.org/clojure.core/str, https://clojuredocs.org/clojure.core/format
- Die Bibliothek `clj-time`: https://github.com/clj-time/clj-time
- Die Bibliothek `chrono`: https://github.com/Saikyun/chrono