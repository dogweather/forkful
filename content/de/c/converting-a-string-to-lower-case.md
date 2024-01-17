---
title:                "Umwandlung eines Strings in Kleinbuchstaben"
html_title:           "C: Umwandlung eines Strings in Kleinbuchstaben"
simple_title:         "Umwandlung eines Strings in Kleinbuchstaben"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/c/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Was & Warum?
Bei der Konvertierung eines Strings in Kleinbuchstaben geht es darum, alle Buchstaben in einem gegebenen String von Großbuchstaben auf Kleinbuchstaben umzuwandeln. Programme tun dies, um Eingabedaten einheitlich zu machen und die Vergleichbarkeit von Strings zu erleichtern.

## Wie geht's?
Verwenden Sie die Funktion `tolower()` aus der Standardbibliothek `<ctype.h>`. Sie akzeptiert einen Buchstaben als Eingabe und gibt den entsprechenden Kleinbuchstaben als Ausgabe zurück. Hier ein Beispielcode:

```C
#include <stdio.h>
#include <ctype.h>

int main() {
    char name[] = "Max Mustermann";
    int i;

    for (i = 0; name[i] != '\0'; i++) {
        name[i] = tolower(name[i]);
    }

    printf("Name: %s\n", name);

    return 0;
}
```

Dieses Programm gibt `max mustermann` aus.

## Tief eintauchen
Früher mussten Programme auf ASCII-Codes zurückgreifen, um eine Konvertierung in Kleinbuchstaben durchzuführen. In der aktuellen Version von C ist dies jedoch nicht mehr nötig, da die Funktion `tolower()` automatisch die entsprechenden Codes für Buchstaben erkennt. Als Alternativen werden oft auch die Funktionen `toupper()` und `stricmp()` verwendet, um Groß- und Kleinbuchstaben zu vergleichen.

Bei der Implementierung von `tolower()` wird der Wert von `c` zunächst an die Funktion `toupper()` weitergegeben, um zu überprüfen, ob es sich um einen Großbuchstaben handelt. Wenn ja, wird die Differenz zwischen dem ASCII-Code für den Großbuchstaben und dem für den Kleinbuchstaben berechnet und auf `c` angewendet, um den Buchstaben in Kleinbuchstaben umzuwandeln.

## Siehe auch
- [Die offizielle Dokumentation der Funktion `tolower()`](https://en.cppreference.com/w/c/string/byte/tolower)
- [Weitere Funktionen aus der Standardbibliothek `<ctype.h>`](https://en.cppreference.com/w/c/string/byte)
- [Ein Beispielprogramm zur Verwendung der Funktionen `tolower()` und `toupper()`](https://www.programiz.com/c-programming/examples/convert-uppercase-lowercase)