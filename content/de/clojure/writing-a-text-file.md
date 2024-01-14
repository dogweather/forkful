---
title:                "Clojure: Eine Textdatei schreiben"
programming_language: "Clojure"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/clojure/writing-a-text-file.md"
---

{{< edit_this_page >}}

# Warum

Das Schreiben von Textdateien ist eine wichtige Fähigkeit für jeden Programmierer. Es ermöglicht uns, Daten in einer einfachen, unformatierten Art und Weise zu speichern und später darauf zuzugreifen. Dies ist besonders nützlich, wenn wir mit großen Datenmengen arbeiten oder unsere Daten für andere Programme zugänglich machen möchten.

# Wie geht man vor

Um eine Textdatei in Clojure zu schreiben, können wir die Funktion `spit` verwenden. Diese Funktion nimmt zwei Argumente entgegen: den Dateipfad und den zu schreibenden Text. Wir können auch das optionale Argument `:append` verwenden, um weitere Texte an eine bereits existierende Datei anzuhängen.

```Clojure
(spit "daten.txt" "Hallo Welt!")
(spit "daten.txt" "Ich bin ein Text" :append true)
```

Nach dem Ausführen dieser Code-Beispiele haben wir eine neue Datei "daten.txt" erstellt, die den Text "Hallo Welt!" und "Ich bin ein Text" enthält.

# Tiefergehende Informationen

Um eine bessere Kontrolle über das Schreiben von Textdateien zu haben, können wir die Funktion `with-open` verwenden. Diese Funktion hilft uns dabei, die Datei automatisch zu schließen, sobald wir fertig sind. Zudem können wir auch angeben, wie wir die Datei öffnen möchten, z.B. "w" für Schreiben oder "r" für Lesen.

```Clojure
(with-open [datei (clojure.java.io/writer "daten.txt" :append true)]
  (.write datei "Ich bin ein weiterer Text"))
```

Mit dieser Methode haben wir mehr Kontrolle über das Öffnen und Schließen von Dateien und können auch mehrere schreibende Operationen hintereinander ausführen.

# Siehe auch

- Clojure Dokumentation zu Textdateien: https://clojuredocs.org/clojure.java.io/write
- HowTo: Schreiben und Lesen von Dateien in Clojure: https://www.baeldung.com/clojure-write-read-file