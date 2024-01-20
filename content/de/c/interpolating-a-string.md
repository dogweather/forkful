---
title:                "Eine Zeichenkette interpolieren"
html_title:           "Arduino: Eine Zeichenkette interpolieren"
simple_title:         "Eine Zeichenkette interpolieren"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/c/interpolating-a-string.md"
---

{{< edit_this_page >}}

# Zeichenketten-Interpolation in C: Eine Kurze Einführung 

## Was und Warum?
Die Interpolation von Zeichenketten ist ein Hilfsmittel zur Erstellung formatierter Zeichenketten. Sie ermöglicht es Programmierern, Variablen direkt in eine Zeichenkette einzubauen und sorgt so für eine effizientere und lesbarere Codierung.

## So geht's:
In C verwenden wir die `printf` Funktion für die Zeichenketten-Interpolation. Hier ist ein einfaches Beispiel:

```C
#include <stdio.h>

int main() {
    char name[] = "John";
    printf("Hallo, %s!\n", name);
    return 0;
}
```

In diesem Code ersetzt `%s` den Wert der `name` Variable, so dass die Ausgabe sein wird: `Hallo, John!`.

## Tiefgang:
Historisch gesehen ist `printf` ein Teil der C-Standardbibliothek und wurde ursprünglich in der Sprache B entwickelt. Alternativen zur Interpolation von Zeichenketten in C können Formatierungsfunktionen wie `sprintf` oder `snprintf` sein. Beachten Sie jedoch, dass die Überschrift `%s` im `printf` eine spezifische Implementierung ist, die auf einem Zeiger auf char basiert und die Zeichenkette bis zum ersten Null-Byte liest.

## Siehe Auch:
Für weitere Informationen über die `printf` Funktion und Zeichenketten-Interpolation in C, siehe:

- [Die offizielle Dokumentation von C's printf](https://www.cplusplus.com/reference/cstdio/printf/)
- [Zeichenketten-Interpolation auf Wikipedia](https://en.wikipedia.org/wiki/String_interpolation)