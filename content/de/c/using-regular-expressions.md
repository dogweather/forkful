---
title:                "C: Verwendung von regulären Ausdrücken"
simple_title:         "Verwendung von regulären Ausdrücken"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/c/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Warum

Regular Expressions, auch bekannt als reguläre Ausdrücke, sind ein äußerst nützliches Konzept in der Welt der Programmierung. Sie bieten die Möglichkeit, Textmuster zu erkennen und zu manipulieren, was das Schreiben von effizientem Code erleichtert. Wenn Sie sich mit der Verarbeitung von Text in Ihren Projekten beschäftigen, ist es unerlässlich, sich mit regulären Ausdrücken vertraut zu machen.

## Wie geht's

Das Einbinden von regulären Ausdrücken in Ihren C-Code ist relativ einfach. Sie benötigen lediglich die Header-Datei "regex.h" und schon können Sie loslegen. Hier ist ein Beispiel, wie Sie eine Zeichenkette nach einem bestimmten Muster durchsuchen und markieren können:

```C
#include <stdio.h>
#include <regex.h>

int main() {
    regex_t regex;
    int result;
    char *text = "Hallo Welt";
    char *pattern = "Welt";

    // Kompilieren und prüfen des regulären Ausdrucks
    result = regcomp(&regex, pattern, 0);
    if (result) {
        printf("Fehler beim Kompilieren von RegExp.\n");
        return 0;
    }

    // Durchsuchen des Textes und Ausgabe der Treffer
    result = regexec(&regex, text, 0, NULL, 0);
    if (!result) {
        printf("Welt gefunden!");
    } else if (result == REG_NOMATCH) {
        printf("Keine Übereinstimmung gefunden.");
    } else {
        printf("Fehler beim Durchsuchen des Textes!");
    }

    // Freigeben des Speichers und aufräumen
    regfree(&regex);

    return 0;
}
```

Die Ausgabe dieses Codes wird "Welt gefunden!" sein, da das Muster "Welt" in der Zeichenkette "Hallo Welt" gefunden wurde. Natürlich gibt es noch viele weitere Funktionen und Möglichkeiten im Umgang mit regulären Ausdrücken. Probieren Sie es aus und experimentieren Sie!

## Tiefergehende Informationen

Reguläre Ausdrücke sind ein großes Thema, daher können wir hier nur einen kleinen Einblick bieten. Wenn Sie sich intensiver damit beschäftigen möchten, gibt es viele Informationen und Tutorials online verfügbar. Hier sind einige hilfreiche Ressourcen:

- [Reguläre Ausdrücke in C](https://www.gnu.org/software/libc/manual/html_node/Regular-Expressions.html)
- [Tutorial für reguläre Ausdrücke in C](https://www.codepug.com/tutorials/c-tutorials/c-regular-expressions)
- [Reguläre Ausdrücke Cheat-Sheet](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/Regular_Expressions/Cheat_Sheet)

## Siehe auch

- [Reguläre Ausdrücke in Java](http://www.java-programmieren.com/regulaere-ausdruecke.php)
- [Einführung in reguläre Ausdrücke für Python](https://www.datacamp.com/community/tutorials/python-regular-expression-tutorial)