---
title:    "Clojure: Das Schreiben einer Textdatei"
keywords: ["Clojure"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/de/clojure/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Warum
Das Schreiben von Textdateien ist eine wichtige Fähigkeit für Programmierer und kann in vielen Situationen nützlich sein. Es ermöglicht das Speichern und Wiederverwenden von Daten sowie das Erstellen von Konfigurationsdateien für Programme.

## Wie geht's
Um eine Textdatei in Clojure zu schreiben, können wir die `with-open` Funktion verwenden, um eine Datei zu öffnen und sie mit Inhalt zu füllen. Hier ist ein Beispielcode, der eine Datei mit dem Inhalt "Hallo Welt!" erstellt.

```Clojure
(with-open [file (clojure.java.io/writer "meine-datei.txt")]
  (.write file "Hallo Welt!"))
```

Die `with-open` Funktion sorgt dafür, dass die Datei am Ende automatisch geschlossen wird, um mögliche Speicherlecks zu vermeiden. Sie können auch `println` verwenden, um einen Zeilenumbruch am Ende Ihres Textes hinzuzufügen.

## Tiefer einsteigen
Es gibt verschiedene Optionen, um den Inhalt einer Datei zu ändern oder zu ergänzen, wie zum Beispiel die `append` Funktion. Sie können auch spezifische Zeichenfolgen suchen und ersetzen oder die Größe der Datei ändern. Weitere Informationen finden Sie in der offiziellen Clojure-Dokumentation zu Dateien.

## Siehe auch
- Offizielle Dokumentation zu Dateien in Clojure: https://clojure.org/reference/io
- Nützliche Funktionen aus der Clojure core library: https://clojure.org/api/cheatsheet
- Praktische Anwendungen des Schreibens von Textdateien: https://www.baeldung.com/java-write-to-file