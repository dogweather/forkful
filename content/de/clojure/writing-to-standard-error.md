---
title:                "Schreiben auf Standardfehler"
html_title:           "Clojure: Schreiben auf Standardfehler"
simple_title:         "Schreiben auf Standardfehler"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/clojure/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Was & Warum?
Das Schreiben in den Standardfehler ist eine Methode, um Fehlermeldungen direkt an den Benutzer zu senden. Programmierer nutzen dies, um wichtige Fehlerinformationen ohne Veränderung des regulären Programmausganges anzuzeigen.

## So geht's:
```Clojure
(defn print-stderr [msg]
  (.println System/err msg))

(print-stderr "Diese Nachricht wird in den Standardfehler geschrieben")
```

Ausgabe:
```
Diese Nachricht wird in den Standardfehler geschrieben
```

## Tiefergehende Informationen:
Das Schreiben in den Standardfehler wird oft in der Entwicklung von Kommandozeilenanwendungen verwendet. Es gibt auch die Möglichkeit, in den Standardausgang zu schreiben, jedoch wird hierbei die Ausgabe möglicherweise mit anderen Inhalten vermischt. Eine Alternative zum Schreiben in den Standardfehler ist die Verwendung von Logdateien für Fehlermeldungen. Die Implementierung erfolgt in Clojure mit der Methode ```System/err``` aus der Java-Standardbibliothek.

## Siehe auch:
Offizielle Dokumentation zur Clojure Methode "System/err": <link>
Ein Artikel über die Verwendung von Logdateien in Clojure: <link>
Java Dokumentation zur Klasse "System": <link>