---
title:                "C: String in Kleinbuchstaben umwandeln"
simple_title:         "String in Kleinbuchstaben umwandeln"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/c/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Warum

Eine häufige Aufgabe beim Programmieren ist es, Texte in Groß- oder Kleinschreibung zu konvertieren. Dies kann aus verschiedenen Gründen notwendig sein, zum Beispiel um eine einheitliche Darstellung zu gewährleisten oder um eine Zeichenkette bei der Verarbeitung zu vergleichen.

## Wie es geht

Hier ist ein Beispiel, wie man eine Zeichenkette in Kleinbuchstaben umwandeln kann:

```C
#include <stdio.h>
#include <string.h>
#include <ctype.h>

int main(void) {
    char str[] = "HELLO WORLD";
    int len = strlen(str);
    int i;

    for (i = 0; i < len; i++){
        str[i] = tolower(str[i]);
    }

    printf("%s\n", str);

    return 0;
}
```

Die Ausgabe dieses Codes lautet "hello world". Zunächst importieren wir die benötigten Bibliotheken `stdio.h`, `string.h` und `ctype.h`. Dann definieren wir die Zeichenkette, die wir konvertieren möchten, und ihre Länge. In der `for`-Schleife durchlaufen wir alle Zeichen der Zeichenkette und wenden die Funktion `tolower()` auf jedes Zeichen an, um es in einen Kleinbuchstaben umzuwandeln. Schließlich geben wir die konvertierte Zeichenkette aus.

## Tiefergehender Einblick

Beim Konvertieren von Zeichenketten in Kleinbuchstaben gibt es einige Dinge zu beachten. Zum Beispiel müssen wir sicherstellen, dass die Zeichenkette genügend Speicherplatz hat, um alle konvertierten Zeichen zu speichern. Außerdem kann es nützlich sein zu überprüfen, ob die Zeichenkette bereits nur aus Kleinbuchstaben besteht, um unnötige Konvertierungen zu vermeiden.

Eine alternative Möglichkeit, eine Zeichenkette in Kleinbuchstaben zu konvertieren, besteht darin, die Funktion `strlwr()` aus der `string.h` Bibliothek zu verwenden. Diese Funktion arbeitet ähnlich wie die `tolower()` Funktion, wandelt jedoch die gesamte Zeichenkette in Kleinbuchstaben um.

## Siehe auch

- [Online Compiler, um den Code selbst auszuprobieren](https://www.onlinegdb.com/)
- [Weitere Informationen zu den Funktionen `strncmp()` und `strlwr()`](https://www.tutorialspoint.com/c_standard_library/c_function_strncmp.htm)
- [Dokumentation zur `string.h` Bibliothek](https://www.tutorialspoint.com/c_standard_library/string_h.htm)