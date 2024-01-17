---
title:                "Eine Textdatei schreiben"
html_title:           "C: Eine Textdatei schreiben"
simple_title:         "Eine Textdatei schreiben"
programming_language: "C"
category:             "C"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/c/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Was ist das und warum?
Das Schreiben einer Textdatei ist eine grundlegende und wichtige Fähigkeit für C-Programmierer. Es ermöglicht uns, Daten dauerhaft zu speichern und später wieder darauf zuzugreifen. Textdateien sind in einfachen und lesbaren Formaten geschrieben, was sie ideal für die Kommunikation zwischen verschiedenen Programmen macht.

## Wie geht's?
Um eine Textdatei in C zu schreiben, müssen wir zuerst die Header-Datei `stdio.h` einbinden. Dann nutzen wir die Funktion `fprintf()`, um Daten in die Textdatei zu schreiben. Hier ist ein Beispiel, das den Text "Hello World!" in eine Datei namens `output.txt` schreibt:

```C
#include <stdio.h>

int main() {
  FILE *file_ptr;
  
  // Öffne die Datei im Schreibmodus
  file_ptr = fopen("output.txt", "w");

  // Schreibe den Text in die Datei
  fprintf(file_ptr, "Hello World!");

  // Schließe die Datei
  fclose(file_ptr);
  return 0;
}
```

Die Datei wird im gleichen Verzeichnis wie das Programm erstellt. Wenn die Datei bereits existiert, wird sie überschrieben.

## Tiefgehende Infos
Textdateien werden seit den früheren Versionen von C verwendet, um einfache Daten zu speichern. Für komplexere Anwendungen können wir jedoch auch binäre oder strukturierte Dateien verwenden. Eine alternative Methode zum Schreiben von Textdateien ist die `fputs()` Funktion, die einzelne Zeichen oder Zeichenfolgen in eine Datei schreibt.

Es ist wichtig, die Datei nach dem Schreiben zu schließen, da sonst möglicherweise nicht alle Daten geschrieben werden. Außerdem müssen wir die Datei im richtigen Modus öffnen, um auch Lese- oder Anhängeoperationen durchführen zu können.

## Weitere Links
- [Die offizielle C-Sprachreferenz](https://devdocs.io/c/)
- [Ein ausführliches Tutorial zum Schreiben von Textdateien in C](https://www.tutorialspoint.com/c_standard_library/c_function_fprintf.htm)