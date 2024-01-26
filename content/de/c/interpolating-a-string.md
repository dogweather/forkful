---
title:                "Zeichenketten interpolieren"
date:                  2024-01-20T17:50:25.195116-07:00
model:                 gpt-4-1106-preview
simple_title:         "Zeichenketten interpolieren"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/c/interpolating-a-string.md"
---

{{< edit_this_page >}}

## What & Why?
String-Interpolation ermöglicht das Einsetzen von Variablenwerten innerhalb eines Strings. Programmierer nutzen diese, um dynamische Textausgaben zu erzeugen und Code lesbarer zu gestalten.

## How to:
C hat keine eingebaute String-Interpolation wie höhere Programmiersprachen. Stattdessen verwendet man `sprintf()` oder `printf()` Funktionen. Hier einige Beispiele:

```C
#include <stdio.h>

int main() {
    char name[] = "Welt";
    int age = 42;

    // Mit printf direkt ausgeben
    printf("Hallo, %s! Du bist %d Jahre alt.\n", name, age);

    // Verwenden von sprintf um den String zu speichern
    char greeting[50];
    sprintf(greeting, "Hallo, %s!", name);
    printf("%s\n", greeting);

    return 0;
}
```

Sample Output:
```
Hallo, Welt! Du bist 42 Jahre alt.
Hallo, Welt!
```

## Deep Dive
In den Anfangstagen von C, vor `sprintf` und `printf`, mussten alle Strings manuell zusammengefügt werden. Mit C99 kamen die variadic functions, die eine variierende Anzahl von Argumenten akzeptieren. Heute verwenden wir Format-Specifier wie `%s` für Strings und `%d` für Integer in Verbindung mit `printf()`-Familienfunktionen zur String-Interpolation.

Alternative Methoden sind:
- `snprintf()`: sicherer, da Überläufe verhindert werden können
- Bibliotheken wie `asprintf()` auf einigen Systemen verfügbar

Intern verwendet `printf()` eine ziemlich komplexe Mechanik, um Typen zu überprüfen und die entsprechende Repräsentation zu erstellen. Es ist wichtig zu verstehen, dass Fehlentsprechungen zwischen Format-Specifiern und den gegebenen Typen zu undefiniertem Verhalten führen können.

## See Also
- C Standard Library reference on `printf`: https://en.cppreference.com/w/c/io/fprintf
- GNU C Library documentation over `printf()`: https://www.gnu.org/software/libc/manual/html_node/Formatted-Output-Functions.html
- Tutorial on variadic functions in C: https://www.cprogramming.com/tutorial/c/lesson17.html
