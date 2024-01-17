---
title:                "Einen Textdatei lesen"
html_title:           "C: Einen Textdatei lesen"
simple_title:         "Einen Textdatei lesen"
programming_language: "C"
category:             "C"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/c/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Was & Warum?
Das Lesen einer Textdatei ist ein häufiger Vorgang in der C-Programmierung. Dabei handelt es sich einfach darum, den Inhalt einer Textdatei in das C-Programm einzulesen und darauf zugreifen zu können. Programmierer nutzen dies, um Daten zu verarbeiten oder Informationen aus Dateien zu lesen.

## So geht's:
Um eine Textdatei in C zu lesen, verwenden wir die Funktion `fopen()` und die dazugehörigen Funktionen `fscanf()` oder `fgets()`. Hier ist ein Beispiel, das zeigt, wie wir eine Datei öffnen, lesen und schließen können:

```C
#include <stdio.h>
int main() {
  FILE *fp;
  char buffer[50];

  // Öffne die Datei
  fp = fopen("beispiel.txt", "r");

  // Lese die Datei Zeile für Zeile mit fgets
  while (fgets(buffer, 50, fp)) {
    printf("%s", buffer);
  }

  // Schließe die Datei
  fclose(fp);
  return 0;
}
```

Dies wird den Inhalt der Datei `beispiel.txt` ausgeben, Zeile für Zeile.

## Tiefer ins Detail:
Lesen von Textdateien ist in der C-Programmierung seit langem eine gängige Praxis, da die Verarbeitung von Textdaten eine wichtige Aufgabe für viele Programme ist. Es gibt auch alternative Methoden zum Lesen von Dateien, wie zum Beispiel das Einlesen des gesamten Inhalts in einen Puffer mit `fread()` oder das Lesen zeichenweise mit `getc()`. Letztendlich kommt es jedoch auf die spezifischen Anforderungen und Präferenzen des Programmierers an.

Es ist auch wichtig zu beachten, dass das Lesen von Dateien ein sicherheitsrelevanter Aspekt ist, da fehlerhafte Eingaben oder unerwartete Dateistrukturen zu unerwünschtem Verhalten führen können. Daher ist es immer ratsam, geeignete Fehlerbehandlungen und Validierungen zu implementieren.

## Siehe auch:
- [C Dateizugriff](https://www.programiz.com/c-programming/c-file-input-output)
- [Standard-C-Bibliothek - Dateien](https://en.cppreference.com/w/c/io)
- [LMGTFY - Wie man in C eine Datei liest](https://lmgtfy.app/?q=How+to+read+a+file+in+C)