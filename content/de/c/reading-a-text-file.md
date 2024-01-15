---
title:                "Lese einer Textdatei"
html_title:           "C: Lese einer Textdatei"
simple_title:         "Lese einer Textdatei"
programming_language: "C"
category:             "C"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/c/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Warum
Wenn du eine Textdatei in deinem C-Programm lesen möchtest, könnte dieses Textdokument wichtige Eingabedaten für dein Programm enthalten oder als Teil einer größeren Datenbank dienen. In diesem Artikel werde ich dir zeigen, wie du eine Textdatei in C einlesen und verarbeiten kannst.

## Wie geht das?
Um eine Textdatei in C zu lesen, musst du zuerst die Datei öffnen. Dies wird mithilfe der Funktion `fopen()` erreicht, die als Parameter den Namen der Datei und den Modus, in dem sie geöffnet werden soll, erwartet. Zum Beispiel:
```C
FILE *datei = fopen("beispiel.txt", "r");
```
Der Modus `"r"` steht für "read" und gibt an, dass die Datei im Lese-Modus geöffnet wird. Anschließend kannst du die Funktion `fscanf()` verwenden, um die Daten aus der Datei einzulesen. Diese Funktion erwartet als Parameter den Datei-Pointer, das Format der Daten, die gelesen werden sollen, und die Variablen, in die die Daten gespeichert werden sollen. Zum Beispiel:
```C
int nummer;
fscanf(datei, "%d", &nummer);
```
Dieser Code liest eine Zahl aus der Datei und speichert sie in der Variable `nummer`.

Wenn du alle Daten aus der Datei gelesen hast, solltest du die Datei schließen, indem du die Funktion `fclose()` verwendest. Dies ist wichtig, um Speicherlecks zu vermeiden.

## Deep Dive
Beim Lesen einer Textdatei ist es wichtig, das Format der Daten zu beachten. Wenn zum Beispiel deine Datei aus mehreren Zeilen besteht und jede Zeile unterschiedliche Daten enthält, musst du verschiedene Formate für `fscanf()` verwenden, um korrekt zu lesen. Außerdem können Fehler beim Einlesen der Daten auftreten, wie zum Beispiel, wenn die angegebenen Formate nicht mit den tatsächlichen Daten in der Datei übereinstimmen. Es ist daher wichtig, Fehlerbehandlung in dein Programm einzubauen, um solche Probleme zu erkennen und zu beheben.

## Siehe auch
- [C-Programmierung: Einführung und Grundlagen](https://www.dereintrachtistrueb.de/wp-content/uploads/2016/11/C-Programmierung.pdf)
- [C File-I/O](https://www.tutorialspoint.com/cprogramming/c_file_io.htm)
- [fscanf() Funktion](https://www.programiz.com/c-programming/library-function/stdio.h/fscanf)