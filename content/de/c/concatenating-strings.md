---
title:                "Zeichenketten verknüpfen"
date:                  2024-01-20T17:34:12.652404-07:00
model:                 gpt-4-1106-preview
simple_title:         "Zeichenketten verknüpfen"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/c/concatenating-strings.md"
---

{{< edit_this_page >}}

## Was & Warum?

String-Konkatenation ist das Verknüpfen von zwei oder mehreren Zeichenketten (Strings) zu einer neuen, durchgehenden Zeichenkette. Das kommt oft vor, wenn wir dynamische Nachrichten bauen oder Daten formatieren müssen.

## So geht's:

```c
#include <stdio.h>
#include <string.h>

int main() {
    char greeting[20] = "Hallo, ";
    char name[] = "Welt";

    strcat(greeting, name); // "Hallo, Welt" wird erzeugt
    printf("%s\n", greeting); // Ausgabe: Hallo, Welt

    return 0;
}
```

## Tiefgang

Historisch gab es schon immer das Bedürfnis, Zeichenketten zu manipulieren. Die C-Standardbibliothek bietet hierfür Funktionen wie `strcat()` und `strncat()`. Die Nutzung dieser Funktionen verlangt jedoch Vorsicht, weil Überläufe des Ziel-Arrays zu Sicherheitsproblemen führen können. Moderne Alternativen in anderen Sprachen nutzen Builder-Pattern oder überladen den `+`-Operator für eine sicherere und intuitivere Verknüpfung von Strings.

In C sollte das Ziel-Array genügend Speicherplatz reserviert haben, um beide Strings und das Nullterminierungszeichen aufzunehmen. `strncat()` ist eine sicherere Variante, welche die Anzahl der konkatenierten Zeichen limitiert, aber dennoch sollte man aufmerksam die Größe des Buffers prüfen. Fehler beim Umgang mit Zeichenketten können zu Pufferrüberläufen (Buffer Overflows) führen, die in der Vergangenheit oft ausgenutzt wurden.

## Siehe Auch

- ISO/IEC 9899:201x - Arbeitsentwurf des C Standard: http://www.open-std.org/jtc1/sc22/wg14/www/docs/n1570.pdf
- `strcat` - cppreference: https://en.cppreference.com/w/c/string/byte/strcat
- `strncat` - cppreference: https://en.cppreference.com/w/c/string/byte/strncat
- CWE-120: Klassische Pufferüberlaufschwachstelle: https://cwe.mitre.org/data/definitions/120.html