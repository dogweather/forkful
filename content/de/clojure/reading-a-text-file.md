---
title:                "Clojure: Eine Textdatei lesen"
programming_language: "Clojure"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/clojure/reading-a-text-file.md"
---

{{< edit_this_page >}}

# Warum

Das Lesen von Textdateien ist eine grundlegende Fähigkeit, die jeder Clojure-Programmierer beherrschen sollte. Es ermöglicht das Lesen und Verarbeiten von externen Daten in Ihren Programmen, was insbesondere für die Entwicklung von Anwendungen mit umfangreichen Datensätzen von Vorteil ist.

# Wie man das macht

Um eine Textdatei in Clojure zu lesen, können wir die Funktion `clojure.java.io/reader` verwenden. Diese akzeptiert als Argument einen Dateipfad und gibt ein Java-Objekt zurück, das wir dann in Clojure-Funktionen weiterverarbeiten können.

Hier ist ein Beispiel, das eine Textdatei namens "textdatei.txt" liest und jede Zeile auf der Konsole ausgibt:

```Clojure
(with-open [reader (reader "textdatei.txt")]
  (doseq [line (line-seq reader)]
    (println line)))
```

Die Funktion `with-open` öffnet das Dateiobjekt und sorgt dafür, dass es nach Beendigung des Vorgangs ordnungsgemäß geschlossen wird. Dann verwenden wir `line-seq` um die Zeilen der Datei einzeln zu durchlaufen und `println` um sie auszugeben.

Die Ausgabe könnte wie folgt aussehen:

```
Dies ist ein Beispieltext
Der zweite Satz
Schließlich endet die Datei
```

# Tiefere Einblicke

Das Lesen von Textdateien in Clojure kann auch mit Hilfe der `slurp`-Funktion erfolgen, die den gesamten Inhalt der Datei als String zurückgibt. Diese kann nützlich sein, wenn Sie die gesamte Datei auf einmal in einem bestimmten Format verarbeiten möchten.

Eine weitere wichtige Funktion ist die `clojure.string/split` Funktion, die eine Zeichenkette anhand eines Trennzeichens in eine Sequenz von Teilen aufteilt. Sie kann verwendet werden, um eine Textdatei in einzelne Wörter oder Sätze aufzuteilen und diese weiter zu verarbeiten.

# Siehe auch

- Clojure Dokumentation zu `clojure.java.io/reader`: https://clojuredocs.org/clojure.java.io/reader
- Clojure Dokumentation zu `slurp`: https://clojuredocs.org/clojure.core/slurp
- Clojure Dokumentation zu `split`: https://clojuredocs.org/clojure.string/split

Danke fürs Lesen und viel Spaß beim Lesen von Textdateien in Clojure!