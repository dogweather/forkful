---
title:                "Großschreibung eines Strings"
html_title:           "C: Großschreibung eines Strings"
simple_title:         "Großschreibung eines Strings"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/c/capitalizing-a-string.md"
---

{{< edit_this_page >}}

Was ist das Capitalizen einer Zeichenfolge und warum machen Programmierer es?

Das Capitalizen einer Zeichenfolge bedeutet, dass alle Buchstaben in einer Zeichenfolge von Klein- zu Großbuchstaben umgewandelt werden. Programmierer tun dies, um die Lesbarkeit und Konsistenz von Code zu verbessern, da es bestimmte Formatierungsanforderungen gibt, die von Programmiersprachen vorgegeben werden und es einfacher ist, diese einzuhalten, wenn die Zeichenfolgen capitalisiert sind.

So machen Sie es:

```c
#include <stdio.h>
#include <ctype.h>

int main() {
    char string[] = "hallo, welt!";
    int i = 0;

    printf("Vor dem Capitalizen: %s\n", string);

    while (string[i]) {
        string[i] = toupper(string[i]);
        i++;
    }

    printf("Nach dem Capitalizen: %s\n", string);

    return 0;
}
```

Output:

```
Vor dem Capitalizen: hallo, welt!
Nach dem Capitalizen: HALLO, WELT!
```

Tiefgehende Informationen:

Das Capitalizen von Zeichenfolgen ist eine gängige Praxis in der Programmierung. Es stammt aus der Zeit, als Computer noch nicht in der Lage waren, Groß- und Kleinbuchstaben zu unterscheiden - daher war es einfacher, durchgehend Großbuchstaben zu verwenden. Heutzutage gibt es verschiedene Alternativen zum Capitalizen, z. B. das Konvertieren von Zeichenfolgen in Titel- oder Kamelkasus, je nach spezifischen Anforderungen des Codes. Die Implementierung des Capitalizens in C kann auch mit der Funktion `toupper()` in der Header-Datei `<ctype.h>` erfolgen, die jeden übergebenen Buchstaben in einen Großbuchstaben umwandelt.

Siehe auch:

- [Unterschiedliche String Transformationen in C](https://stackoverflow.com/questions/14176122/string-transformations-in-c-uppercase-lowercase-titles) auf Stack Overflow
- [Die Header-Datei <ctype.h>](https://www.tutorialspoint.com/c_standard_library/ctype_h.htm) auf Tutorials Point