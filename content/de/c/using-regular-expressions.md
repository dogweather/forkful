---
title:                "Verwendung regulärer Ausdrücke"
html_title:           "C: Verwendung regulärer Ausdrücke"
simple_title:         "Verwendung regulärer Ausdrücke"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/c/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Was und Warum?
Regular Expressions (kurz: Regex) sind eine nützliche Technik, die es Programmierern ermöglicht, komplexe Suchmuster in Texten zu definieren und zu extrahieren. Sie werden häufig verwendet, um eine bestimmte Sequenz von Zeichen in einem String zu finden oder um Daten von einem bestimmten Format zu validieren. Regex spart Zeit und macht den Code viel übersichtlicher, was für Programmierer oft genug Grund ist, sie zu benutzen.

## Wie:
In C können wir mit Hilfe der regulären Ausdrücke sehr einfach Daten verarbeiten und validieren. Um eine Regex in C zu verwenden, müssen wir das Header-File "regex.h" einbinden. Im folgenden Beispiel suchen wir in einem String nach einer bestimmten Zeichenkette und geben diese aus.

```C
#include <stdio.h>
#include <regex.h>

int main() {
    regex_t regex;
    char *text = "Dies ist ein Beispieltext";
    char *pattern = "Beispiel";

    if (regcomp(&regex, pattern, REG_EXTENDED) != 0) {
        printf("Konnte den regulären Ausdruck nicht kompilieren.\n");
        return 1;
    }

    // Länge des Match-Strings
    size_t nmatch = 1;
    // Array, um die Ergebnisse zu speichern
    regmatch_t pmatch[nmatch];
    // Überprüfen, ob der reguläre Ausdruck im String vorhanden ist
    if (regexec(&regex, text, nmatch, pmatch, 0) == 0) {
        // Ausgabe des gefundenen Matches
        printf("Das Pattern '%s' wurde in '%s' gefunden.\n", pattern, text);
        // Ausgabe des gefundenen Strings
        printf("Der gefundene String ist: '%.*s'.\n", (pmatch[0].rm_eo - pmatch[0].rm_so), &text[pmatch[0].rm_so]);
    } else {
        printf("Kein Match gefunden.\n");
    }

    regfree(&regex);
    return 0;
}
```
Die Ausgabe des obigen Codes wäre:

```
Das Pattern 'Beispiel' wurde in 'Dies ist ein Beispieltext' gefunden.
Der gefundene String ist: 'Beispiel'.
```

## Tiefergehende Information:
Regex gibt es schon seit den 1950er Jahren und wurde in der Sprache SNOBOL von Ken Thompson entwickelt. Heutzutage gibt es auch andere Möglichkeiten, Suchmuster zu definieren und zu extrahieren, wie zum Beispiel mit Hilfe von String-Manipulationsfunktionen oder regulären Ausdrücken in anderen Programmiersprachen, aber Regex ist immer noch eine sehr beliebte Option für viele Programmierer. Die Implementierung von Regex in C ist relativ komplex und erfordert ein Verständnis von regulären Ausdrücken, so dass es manchmal einfacher ist, auf andere Methoden zurückzugreifen.

## Siehe auch:
- [regex.h - C Reference](https://www.gnu.org/software/libc/manual/html_node/POSIX-Regular-Expressions.html)
- [Einführung in reguläre Ausdrücke (Regex) in C](https://www.geeksforgeeks.org/introduction-regular-expressions-in-c/)