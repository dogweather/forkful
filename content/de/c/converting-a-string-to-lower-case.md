---
title:                "Umformung eines Strings in Kleinbuchstaben"
date:                  2024-01-20T17:37:54.619905-07:00
model:                 gpt-4-1106-preview
simple_title:         "Umformung eines Strings in Kleinbuchstaben"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/c/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## What & Why? (Was & Warum?)
Das Umwandeln eines Strings in Kleinbuchstaben macht alle Zeichen einheitlich klein - praktisch für Vergleiche und Datenverarbeitung. Wir programmieren es, um Konsistenz zu schaffen und Groß- und Kleinschreibung zu ignorieren.

## How to (Wie macht man das)
Der Standardansatz in C, um einen String in Kleinbuchstaben zu konvertieren, verwendet die `tolower` Funktion aus der Standardbibliothek `ctype.h`. Hier ist ein einfaches Beispiel:

```c
#include <stdio.h>
#include <ctype.h>

void toLowerCase(char *str) {
    while (*str) {
        *str = tolower((unsigned char) *str);
        str++;
    }
}

int main() {
    char text[] = "Hallo Welt!";
    toLowerCase(text);
    printf("In Kleinbuchstaben: %s\n", text);
    return 0;
}
```

Ausgabe:
```
In Kleinbuchstaben: hallo welt!
```

## Deep Dive (Tiefere Einblicke)
Historisch gesehen ist die Umwandlung von Großbuchstaben in Kleinbuchstaben schon seit den ersten Computersystemen relevant. Der ASCII-Standard legt fest, dass Groß- und Kleinbuchstaben einen festen numerischen Abstand zueinander haben. Alternativ zu `tolower` könntest du auch manuell diesen Abstand nutzen, um die Konvertierung durchzuführen, jedoch ist `tolower` sicherer, da es Prüfungen für nicht-alfabetische Zeichen beinhaltet.

Die `tolower` Funktion nimmt ein `int` als Argument und gibt das entsprechende Kleinbuchstaben `int` zurück, falls das Zeichen ein Buchstabe ist. Sonst bleibt das Zeichen unverändert. In einer Schleife über den String angewendet, konvertiert diese Funktion zuverlässig alle Zeichen. Achte auf Zeichen-Encodings: `tolower` ist nur für ASCII-Zeichen definiert. Für andere Zeichen-Encodings könnten spezielle Bibliotheken oder Funktionen notwendig sein.

## See Also (Siehe auch)
- C Standardbibliothek - `ctype.h`: https://en.cppreference.com/w/c/header/ctype
- ASCII Tabelle und Charakter Kodierung: http://www.asciitable.com/
- Unicode Transformationen mit C: https://unicode.org/faq/programming.html