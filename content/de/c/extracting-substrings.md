---
title:                "Unterstrings extrahieren"
html_title:           "C: Unterstrings extrahieren"
simple_title:         "Unterstrings extrahieren"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/c/extracting-substrings.md"
---

{{< edit_this_page >}}

## Warum

Wenn du schon einmal mit C-Code gearbeitet hast, hast du sicherlich schon einmal die Notwendigkeit gehabt, Teile von Strings zu extrahieren. Das bedeutet, dass du nur einen Teil eines Strings verwenden möchtest, zum Beispiel um nur den Vornamen aus einem vollständigen Namen zu erhalten. In diesem Artikel werden wir uns ansehen, wie man in C Substrings extrahieren kann und warum es ein nützliches Werkzeug in deinem Werkzeugkasten sein kann.

## Wie geht das?

Um Substrings in C zu extrahieren, müssen wir die Bibliotheksfunktion "strncpy" verwenden. Diese Funktion kopiert eine vom Benutzer angegebene Anzahl von Zeichen aus einem String in einen anderen. Das folgende Beispiel zeigt, wie man den Nachnamen "Müller" aus dem vollständigen Namen "Max Müller" extrahiert:

```C
#include <stdio.h>
#include <string.h>

int main() {
    // Definiere den vollständigen Namen
    char name[] = "Max Müller";

    // Bestimme die Länge des vollständigen Namens
    int length = strlen(name);

    // Extrahiere den Nachnamen
    char nachname[10];
    strncpy(nachname, name + 4, length - 4);

    // Drucke den Nachnamen aus
    printf("Nachname: %s\n", nachname);

    return 0;
}
```

Dieses Beispiel nutzt die Funktion "strlen", um die Länge des vollständigen Namens zu bestimmen, und die Funktion "strncpy", um die letzten 4 Zeichen (die Länge des Vornamens) in den Nachnamen zu kopieren. Das Ergebnis sollte "Müller" ausgeben.

## Tiefer in die Materie

Neben "strncpy" gibt es noch andere nützliche Funktionen, die für die Arbeit mit Substrings in C verwendet werden können. Die Funktion "strchr" zum Beispiel sucht nach einem bestimmten Zeichen in einem String und gibt einen Zeiger auf die Stelle zurück, an der es gefunden wurde. Und die Funktion "strstr" sucht nach einem Unterstring in einem String und gibt einen Zeiger auf die Stelle zurück, an der er gefunden wurde. Mit diesen und anderen Funktionen kannst du komplexe Operationen mit Substrings durchführen, wie zum Beispiel das Zusammenführen von Strings oder das Vergleichen von Teilstrings.

Es ist auch wichtig zu beachten, dass die Länge eines Strings in C begrenzt ist. Um Substrings zu extrahieren, solltest du darauf achten, dass die Länge deines Ausgabe-Strings groß genug ist, um alle extrahierten Zeichen aufzunehmen. Andernfalls läufst du Gefahr, dass du Speicherbereiche überschreibst und unerwünschte Ergebnisse erhältst.

## Siehe auch

- ["strncpy" Dokumentation](https://www.cplusplus.com/reference/cstring/strncpy/)
- ["strlen" Dokumentation](https://www.cplusplus.com/reference/cstring/strlen/)
- ["strchr" Dokumentation](https://www.cplusplus.com/reference/cstring/strchr/)
- ["strstr" Dokumentation](https://www.cplusplus.com/reference/cstring/strstr/)
- ["C Strings" Tutorial](https://www.programiz.com/c-programming/c-strings)