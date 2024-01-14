---
title:                "Clojure: Eine Textdatei schreiben"
simple_title:         "Eine Textdatei schreiben"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/clojure/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Warum

Das Schreiben von Textdateien ist eine grundlegende Fähigkeit für jeden, der sich für die Programmierung mit Clojure interessiert. Textdateien ermöglichen eine einfache und effiziente Art, strukturierte Daten zu speichern und zu bearbeiten.

## Wie man eine Textdatei schreibt

Um eine Textdatei in Clojure zu schreiben, können wir die Funktion "spit" verwenden. Diese Funktion akzeptiert zwei Argumente: den Dateinamen, unter dem die Datei gespeichert werden soll, und den Inhalt der Datei als Zeichenfolge. 

```Clojure
(spit "meinText.txt" "Dies ist ein Beispieltext.")
```

Der obige Code erstellt eine Datei mit dem Namen "meinText.txt" und fügt den angegebenen Text als Inhalt hinzu. Um Textzeilen zu einem bestehenden Text hinzuzufügen, können wir die Funktion "with-open" verwenden, um die Datei zu öffnen und dann "write-line" verwenden, um den Text hinzuzufügen. 

```Clojure
(with-open [datei (io/writer "meinText.txt" :append true)]
  (write-line datei "Dies ist eine neue Zeile.")
  (write-line datei "Und eine weitere.")
)
```

Die obige Codeblock wird die Datei "meinText.txt" öffnen und zwei Zeilen hinzufügen, ohne den bereits vorhandenen Inhalt zu überschreiben, da wir "append true" als Option gesetzt haben.

## Tiefergehende Informationen

Das Schreiben von Textdateien in Clojure ist Teil der Standardbibliothek und bietet verschiedene Funktionen, um verschiedene Arten von Dateien zu schreiben, einschließlich CSV-Dateien und XML-Dateien. Es ist auch möglich, die Codierung und das Zeilenende für die geschriebene Datei anzugeben. Informationen zu diesen Funktionen und mehr finden Sie in der offiziellen Dokumentation.

## Siehe auch

- [Offizielle Clojure-Dokumentation zu spit](https://clojuredocs.org/clojure.core/spit)
- [Offizielle Clojure-Dokumentation zu with-open](https://clojuredocs.org/clojure.core/with-open)
- [Weitere Funktionen in der Clojure-Standardbibliothek zum Schreiben von Textdateien](https://clojure.org/reference/io)