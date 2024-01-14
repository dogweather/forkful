---
title:    "C: Einen String großschreiben"
keywords: ["C"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/de/c/capitalizing-a-string.md"
---

{{< edit_this_page >}}

# Warum

In dieser kurzen Blog-Post geht es um das Kapitalisieren von Strings in der Programmiersprache C. Wir werden die verschiedenen Methoden und Techniken betrachten, um dies zu erreichen.

# Wie

Die einfachste Methode, um eine Zeichenfolge in Großbuchstaben zu konvertieren, besteht darin, die Funktion `toupper` aus der Standardbibliothek `ctype.h` zu verwenden. Diese Funktion nimmt einen einzelnen Zeichenwert als Argument und gibt den Großbuchstaben davon zurück.

Beispiel:

```C
#include <stdio.h>
#include <ctype.h>

int main() {
    char str[] = "Hallo";
    printf("Ursprünglicher String: %s\n", str);

    for (int i = 0; str[i] != '\0'; i++) {
        str[i] = toupper(str[i]);
    }

    printf("Kapitalisierter String: %s\n", str);

    return 0;
}
```

Ausgabe:

```
Ursprünglicher String: Hallo
Kapitalisierter String: HALLO
```

Eine andere Möglichkeit ist die Verwendung der Funktion `sprintf` aus der Standardbibliothek `stdio.h`. Diese Funktion kann einen formatierten String in eine Variable schreiben. Durch Verwendung des `%s`-Platzhalters und der Funktion `toupper` können wir die gesamte Zeichenfolge in Großbuchstaben konvertieren.

Beispiel:

```C
#include <stdio.h>
#include <ctype.h>

int main() {
    char str[] = "Hallo";
    printf("Ursprünglicher String: %s\n", str);

    sprintf(str, "%s", str);
    for (int i = 0; str[i] != '\0'; i++) {
        str[i] = toupper(str[i]);
    }

    printf("Kapitalisierter String: %s\n", str);

    return 0;
}
```

Ausgabe:

```
Ursprünglicher String: Hallo
Kapitalisierter String: HALLO
```

# Deep Dive

Es gibt auch andere Methoden und Bibliotheken, um Strings in C zu kapitalisieren. Eine davon ist `strlwr` aus der Bibliothek `string.h`, die die Zeichenfolge in Kleinbuchstaben konvertiert, und anschließend die Funktion `toupper` verwendet, um sie in Großbuchstaben zu konvertieren.

Es ist auch wichtig zu beachten, dass bei der Verwendung von `toupper` nur Standard-ASCII-Zeichen konvertiert werden können. Wenn die Zeichenkodierung UTF-8 verwendet wird, sind spezielle Funktionen wie `utf8_toupper` aus der Bibliothek `utf8proc.h` erforderlich, um Zeichen mit Akzenten oder anderer Diakritikums in Großbuchstaben umzuwandeln.

# Siehe auch

- [Tolower and Toupper in C](https://www.geeksforgeeks.org/tolower-toupper-c/)
- [Convert string to uppercase in C](https://www.includehelp.com/c-programs/convert-string-to-upper-case-c-programming-string-toupper-function.aspx)
- [UTF-8 Processing Library](https://www.npmjs.com/package/utf8proc)