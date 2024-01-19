---
title:                "Eine Textdatei lesen"
html_title:           "Bash: Eine Textdatei lesen"
simple_title:         "Eine Textdatei lesen"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/clojure/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Was & Warum?

Lesen einer Textdatei bedeutet, die Daten aus dieser Datei in Ihr Programm zu laden. Dies ist wichtig, um größere Datenmengen zu verarbeiten oder Daten für die weitere Verarbeitung zu speichern.

## Wie geht das?

Hier ist ein einfacher Weg, eine Textdatei in Clojure zu lesen:

```Clojure
(with-open [rdr (java.io.BufferedReader. 
      (java.io.FileReader. "pfad/zu/ihrer/datei.txt"))]
  (doseq [line (line-seq rdr)]
    (println line)))
```

Lassen Sie uns auf die Ausgabe schauen:

```Clojure
"Das ist Zeile eins deiner Textdatei."
"Und das ist Zeile zwei."
"Now we're on line three."
```

## Vertiefter Einblick

Die obige Methode ist einfach und effektiv, hat aber ihre Wurzeln in Java. Da Clojure auf Java läuft, nutzen wir hier die Java-Bibliotheken für Dateizugriffe. Eine Alternative besteht darin, die Clojure-Bibliothek `clojure.java.io` zu verwenden. Sie können aber auch auf niedrigere Ebenen gehen und die `java.nio.file` API verwenden.

Das Wichtigste an dieser Methode ist, dass sie sicherstellt, dass die Datei nach dem Lesen geschlossen wird, indem sie 'with-open' verwendet. Dies verhindert Speicherlecks und andere Probleme, die auftreten können, wenn Dateien offen gelassen werden.

## Siehe auch

Da wir hier nur die Grundlagen abdecken können, empfehlen wir Ihnen folgende Ressourcen für weitere Details:

1. [Official Clojure Documentation](https://clojure.org/reference/reader)
2. [Java's BufferedReader Class](https://docs.oracle.com/javase/8/docs/api/java/io/BufferedReader.html)
3. [How to read files in Clojure](https://www.baeldung.com/clojure-read-file)
4. [clojure.java.io API](https://clojure.github.io/clojure/clojure.java.io-api.html)