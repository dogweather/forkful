---
title:                "Einen Textdatei lesen"
html_title:           "Bash: Einen Textdatei lesen"
simple_title:         "Einen Textdatei lesen"
programming_language: "Bash"
category:             "Bash"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/bash/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Was ist das und warum machen wir es?

Das Lesen einer Textdatei ist das Abrufen von Informationen aus einer Datei, die Text enthält. Programmierer nutzen dies, um Daten zu erhalten, die sie in ihren Programmen verwenden können.

## Wie geht's?

Hier sind zwei Techniken, um eine Textdatei in Bash zu lesen:

- Mit dem Befehl `cat` können wir den gesamten Inhalt einer Datei auf dem Bildschirm ausgeben.
    
    ```Bash
    cat dateiname
    ```
    Output:
    ```Bash
    Dies ist ein Beispieltext.
    ```

- Mit dem Befehl `while read` können wir Zeile für Zeile durch eine Datei iterieren und den Inhalt jeder Zeile verarbeiten.
    
    ```Bash
    while read zeile; do
        # hier kannst du code schreiben, um jede zeile zu verarbeiten
        echo $zeile
    done < dateiname
    ```
    Output:
    ```Bash
    Dies ist Zeile 1.
    Dies ist Zeile 2.
    ```

## Tiefer tauchen

In der Geschichte der Programmierung wurden verschiedene Techniken verwendet, um Textdateien zu lesen. Eine beliebte Methode war die Verwendung von `awk`. Heutzutage gibt es auch alternative Methoden, wie z.B. das Lesen von Textdateien in anderen Programmiersprachen oder die Verwendung spezialisierter Tools wie `sed` oder `grep`.

Um eine Textdatei in Bash zu lesen, werden sogenannte File Descriptors verwendet. Der Standardfile Descriptor ist 0, der den Befehl `stdin` verwendet, um Daten von der Tastatur oder einer anderen Quelle zu lesen.

## Siehe auch

- [Bash-Handbuch zu Grundlagen des Lesens von Dateien](https://www.gnu.org/software/bash/manual/html_node/Reading-Files.html)
- [Bash-Referenzhandbuch für den Befehl `while read`](https://ss64.com/bash/read.html)