---
title:                "Eine Textdatei schreiben"
html_title:           "Arduino: Eine Textdatei schreiben"
simple_title:         "Eine Textdatei schreiben"
programming_language: "C"
category:             "C"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/c/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Was & Warum?
Das Schreiben einer Textdatei bedeutet, Daten in einer lesbaren Form auf dem Dateisystem zu speichern. Programmierer tun dies, um Daten persistent zu machen, sie zu übertragen oder für spätere Verarbeitungen zu sichern.

## Anleitung:
Hier ist ein einfaches Beispiel, wie man eine Textdatei in C erstellt und in sie schreibt:

```C
#include <stdio.h>

int main() {
    FILE *datei = fopen("beispiel.txt", "w");
    if (datei == NULL) {
        printf("Datei konnte nicht erstellt werden.\n");
        return 1;
    }

    fprintf(datei, "Hallo, Welt!\n");
    fclose(datei);

    return 0;
}
```
Das Programm erstellt eine Textdatei namens "beispiel.txt" und schreibt "Hallo, Welt!" hinein. Überprüfe das Verzeichnis, um die erstellte Datei zu sehen.

## Tiefgang:
Historisch betrachtet verwenden Betriebssysteme wie Unix und Windows verschiedene Methoden zum Umgang mit Dateien, aber die C-Standardbibliothek abstrahiert diese Details durch das `FILE`-Objekt. Alternative Ansätze umfassen die Verwendung von Systemaufrufen wie `write` unter Unix oder `WriteFile` unter Windows. Bei der Implementierung ist zu berücksichtigen, dass Fehler beim Öffnen, Schreiben oder Schließen einer Datei auftreten können, die ordnungsgemäß behandelt werden sollten.

## Siehe Auch:
- [C Standard Library - File Handling](https://en.cppreference.com/w/c/io)
- [GNU C Library – Input/Output on Streams](https://www.gnu.org/software/libc/manual/html_node/I_002fO-Overview.html)
- [Stackoverflow - Writing Files in C](https://stackoverflow.com/questions/tagged/file-io+c)