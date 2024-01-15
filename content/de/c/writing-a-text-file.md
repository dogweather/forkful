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

## Warum

Wenn du mit C programmierst, kennst du wahrscheinlich bereits die Verwendung von Konsolenausgaben mit `printf` oder `cout`. Aber hast du schon einmal daran gedacht, eine Datei mit deinem C-Code zu erstellen? Das Schreiben von Textdateien kann sehr nützlich sein, um Daten auf lange Sicht zu speichern oder um mit anderen Programmen zu interagieren.

## Wie es geht

Das Schreiben einer Textdatei in C kann auf verschiedene Arten erfolgen, aber hier sind einige Beispiele, um dir den Einstieg zu erleichtern:

```C
#include <stdio.h>

int main() {

    // Öffne eine Datei im “write” Modus
    FILE *file = fopen("textdatei.txt", "w");

    // Überprüfe, ob die Datei erfolgreich geöffnet wurde
    if (file == NULL) {
        printf("Fehler beim Öffnen der Datei!");
        return 1;
    }

    // Schreibe Text in die Datei
    fputs("Dies ist eine Textdatei, geschrieben mit C!", file);

    // Schließe die Datei
    fclose(file);

    return 0;
}
```

Dieses Beispiel öffnet eine Datei namens "textdatei.txt" im Schreibmodus und fügt den Text "Dies ist eine Textdatei, geschrieben mit C!" hinzu. Beachte, dass du die Datei immer schließen musst, nachdem du damit fertig bist.

Du kannst auch mit dem `fprintf` Befehl formatierten Text in eine Datei schreiben:

```C
fprintf(file, "Dies ist eine Zahl: %d", 10);
```

Dies schreibt den formatierten Text in die Datei, in diesem Fall "Dies ist eine Zahl: 10". Du kannst alle gängigen printf-Spezifikationen verwenden, um den Text zu formatieren.

## Tiefergehende Details

Das Schreiben von Textdateien in C erfordert die Verwendung der `FILE` Struktur und die Funktionen `fopen`, `fclose` und `fprintf` oder `fputs`. Diese Funktionen sind in der Header-Datei "stdio.h" deklariert. Du kannst auch den dritten Parameter von `fopen` verwenden, um den Zeichenkodierungstyp der Datei anzugeben, z.B. "w, UTF-8" oder "w, ISO-8859-1". Es ist auch wichtig, zu überprüfen, ob das Öffnen oder Schreiben der Datei erfolgreich war, da dies zu Laufzeitfehlern führen kann.

## Siehe auch

- Die Dokumentation zu [FILE in der C-Referenz](https://www.tutorialspoint.com/c_standard_library/c_function_fopen.htm)
- Eine [ausführliche Anleitung zum Schreiben von Textdateien in C](https://www.tutorialspoint.com/cprogramming/c_file_io.htm)
- Eine [Einführung in C](https://www.programiz.com/c-programming) auf Programiz.