---
title:                "C: Verwendung von regulären Ausdrücken"
programming_language: "C"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/c/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Warum

Reguläre Ausdrücke, auch bekannt als "Regex", sind ein mächtiges Werkzeug für jeden Programmierer. Sie erlauben es uns, komplexe Muster in Texten zu erkennen und zu manipulieren. Dies kann sehr nützlich sein, zum Beispiel bei der Validierung von Benutzereingaben oder beim Extrahieren von Daten aus Dateien.

## Wie man Reguläre Ausdrücke verwendet

Die Verwendung von regulären Ausdrücken erfordert einige Kenntnisse in der Programmiersprache C. Um zu beginnen, müssen wir die Bibliothek "regex.h" einbinden. Dann können wir ein neues Regex-Objekt erstellen und es mit unserem gewünschten Muster initialisieren. Anschließend können wir in einer Schleife durch den Text gehen und mit Hilfe von Funktionen wie "regexec()" überprüfen, ob das Muster übereinstimmt.

Hier ist ein einfaches Beispiel, bei dem wir eine E-Mail-Adresse aus einem Text extrahieren:

```C
#include <regex.h>
#include <stdio.h>
#include <stdlib.h>

int main() {
    regex_t regex;
    char *text = "Meine E-Mail-Adresse ist max.mustermann@test.com";
    char *pattern = "([a-z0-9._%+-]+@[a-z0-9.-]+\\.[a-z]{2,})";

    if (regcomp(&regex, pattern, REG_EXTENDED) != 0) {
        printf("Fehler beim Kompilieren des regulären Ausdrucks\n");
        exit(1);
    }

    regmatch_t match[2];
    if (regexec(&regex, text, 2, match, 0) == 0) {
        // Das erste Element in match enthält den gesamten Treffer
        // Das zweite Element enthält die E-Mail-Adresse, die wir extrahieren wollen
        char *email = malloc(sizeof(char) * (match[1].rm_eo - match[1].rm_so + 1));
        sprintf(email, "%.*s", match[1].rm_eo - match[1].rm_so, text + match[1].rm_so);

        printf("Gefundene E-Mail-Adresse: %s\n", email);
        free(email);
    } else {
        printf("Keine E-Mail-Adresse gefunden\n");
    }

    regfree(&regex);
    return 0;
}

/* Ausgabe:
Gefundene E-Mail-Adresse: max.mustermann@test.com
*/
```

## Tiefergehende Informationen

Reguläre Ausdrücke können kompliziert werden, wenn man mehrere Regeln und Sonderzeichen gleichzeitig verwenden möchte. Zum Glück gibt es viele Ressourcen, die uns dabei helfen können, diese Muster zu verstehen und zu erstellen. Eine der nützlichsten Funktionen ist die Verwendung von sogenannten "Capturing Groups", die es uns ermöglichen, Teile des Treffers zu speichern und später darauf zuzugreifen.

Außerdem ist es wichtig zu beachten, dass reguläre Ausdrücke immer nur auf Text angewendet werden können. Das bedeutet, dass wir den Inhalt von Dateien zunächst in eine Zeichenkette konvertieren müssen, bevor wir die Ausdrücke verwenden können.

## Siehe auch

- [Regex-Tutorial für C-Programmierer](https://www.regular-expressions.info/c.html)
- [Offizielle Dokumentation für die "regex.h" Bibliothek](https://www.gnu.org/software/libc/manual/html_node/Regular-Expressions.html)
- [Ein praktisches Cheat-Sheet für reguläre Ausdrücke in C](https://www.cheatography.com/davechild/cheat-sheets/regular-expressions/)