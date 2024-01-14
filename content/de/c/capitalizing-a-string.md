---
title:                "C: Großschreibung eines Zeichens:"
simple_title:         "Großschreibung eines Zeichens:"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/c/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Was ist das Ziel des Großschreibung von Zeichenfolgen?

Das Ziel des Großschreibens von Zeichenfolgen ist es, die Darstellung von Text zu verbessern und ihn leichter lesbar zu machen. Durch die Verwendung von Groß- und Kleinschreibung können wichtige Wörter oder Passagen hervorgehoben werden, um die Bedeutung des Textes zu verdeutlichen.

## Wie funktioniert die Großschreibung in C?

In der Programmiersprache C gibt es verschiedene Methoden, um Zeichenfolgen zu verändern. Eine Möglichkeit ist die Verwendung der Funktion `toupper()`, welche jeden Buchstaben einer Zeichenfolge in Großbuchstaben umwandelt. Diese Funktion benötigt als Argument den ASCII-Wert des zu ändernden Zeichens.

Ein Beispiel für die Verwendung von `toupper()` wäre folgender Code:

```C
#include <stdio.h>
#include <ctype.h>

int main(void)
{
    char s[50];
    
    printf("Geben Sie eine Zeichenfolge ein: ");
    fgets(s, 50, stdin);
    
    for (int i = 0; s[i] != '\0'; i++)
    {
        s[i] = toupper(s[i]);
    }
    
    printf("Die Großschreibung der eingegebenen Zeichenfolge lautet: %s\n", s);
    
    return 0;
}
```

**Ausgabe:**

```
Geben Sie eine Zeichenfolge ein: Hallo Welt
Die Großschreibung der eingegebenen Zeichenfolge lautet: HALLO WELT
```

Es ist wichtig zu beachten, dass die Funktion `toupper()` nur Buchstaben in der ASCII-Range konvertiert. Zeichen wie Zahlen oder Sonderzeichen bleiben unverändert.

## Tiefergehende Informationen über die Großschreibung

Bei der Großschreibung von Zeichenfolgen gibt es noch weitere Aspekte zu beachten. Zum Beispiel gibt es in der deutschen Sprache Buchstaben, die mit bestimmten diakritischen Zeichen wie Umlauten und Akzenten versehen sind. Diese werden bei der Konvertierung zu Großbuchstaben in ASCII-Werten oft nicht korrekt umgewandelt.

Ein weiterer wichtiger Punkt ist die Unterstützung von Unicode in modernen Programmiersprachen wie C. Unicode ermöglicht die Darstellung von Zeichen aus verschiedenen Sprachen und Schriftsystemen, die außerhalb der ASCII-Range liegen. Um sicherzustellen, dass die Großschreibung in Unicode korrekt funktioniert, müssen geeignete Bibliotheken eingebunden werden.

## Siehe auch

- [ASCII-Tabelle](https://www.ascii-code.com/)
- [Unicode-Bibliothek für C](https://www.gnu.org/software/libunistring/)