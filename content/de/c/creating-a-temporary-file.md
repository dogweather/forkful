---
title:                "C: Ein temporäres File erstellen"
programming_language: "C"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/c/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Warum

In der Programmierung gibt es oft die Notwendigkeit, temporäre Dateien zu erstellen. Diese dienen als Zwischenspeicher für Daten, die für einen bestimmten Zeitraum benötigt werden. In diesem Blog-Post werden wir uns genauer mit dem Erstellen von temporären Dateien in C beschäftigen.

## Wie erstelle ich eine temporäre Datei?

Das Erstellen einer temporären Datei ist in C relativ einfach. Wir verwenden die Funktion `tmpfile()`, die Teil der Standardbibliothek `stdio.h` ist. Diese Funktion erstellt eine temporäre Datei und gibt einen Datei-Stream zurück, auf den wir schreiben oder lesen können.

Im folgenden Beispiel erstellen wir eine temporäre Datei und schreiben einige Daten in sie:

```C
#include <stdio.h>

int main() {
  FILE *temp = tmpfile();
  if(temp == NULL) {
    printf("Fehler beim Erstellen der temporären Datei.");
    return 1;
  }

  // Daten in die temporäre Datei schreiben
  fprintf(temp, "Dies ist eine temporäre Datei.");

  // Datei schließen und löschen
  fclose(temp);
  remove(temp); // Diese Zeile ist optional

  return 0;
}
```

Das obige Beispiel verwendet den Datei-Stream und die Funktion `fprintf()` aus der Standardbibliothek `stdio.h`, um Daten in die Datei zu schreiben. Anschließend wird die Datei geschlossen und optional auch gelöscht, indem wir die Funktion `remove()` verwenden.

## Tiefergehende Informationen

Wenn wir eine temporäre Datei erstellen, wird sie im Verzeichnis `/tmp` auf Unix-Systemen oder im aktuellen Arbeitsverzeichnis auf Windows-Systemen erstellt. Die Datei wird automatisch gelöscht, wenn sie geschlossen wird oder das Programm beendet wird. Auch können wir eine Datei mit einem bestimmten Namen erstellen, indem wir die Funktion `tmpnam()` verwenden.

Es ist wichtig zu beachten, dass temporäre Dateien für die einfache Zwischenspeicherung von Daten gedacht sind und nicht für dauerhafte Speicherung. Sie sind auch nicht für die gemeinsame Nutzung von Daten zwischen verschiedenen Programmen gedacht. In solchen Fällen sollten andere Methoden der Kommunikation und/oder des Datenaustauschs verwendet werden.

## Siehe auch

- [Die offizielle Dokumentation zur Funktion `tmpfile()`](https://www.cplusplus.com/reference/cstdio/tmpfile/)
- [Weitere Informationen zur Verwendung von temporären Dateien in C](https://www.geeksforgeeks.org/temporary-file-in-c-programming/)
- [Übersicht der Standardbibliothek `stdio.h`](https://www.tutorialspoint.com/c_standard_library/stdio_h.htm)