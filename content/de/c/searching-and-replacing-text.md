---
title:                "Suchen und Ersetzen von Text"
date:                  2024-01-20T17:57:11.779339-07:00
model:                 gpt-4-1106-preview
simple_title:         "Suchen und Ersetzen von Text"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/c/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Was & Warum?
Im Kern geht es beim Suchen und Ersetzen von Text darum, eine Zeichenkette in einem größeren Textblock zu finden und sie durch eine andere zu ersetzen. Programmierer nutzen das, um Daten zu aktualisieren, Fehler zu korrigieren oder Massenbearbeitungen durchzuführen.

## How To / Wie geht's?
Hier ist ein einfacher C-Code, der illustriert, wie man eine Funktion schreibt, um ein Wort in einem String zu suchen und zu ersetzen:

```C
#include <stdio.h>
#include <string.h>

void searchAndReplace(char *source, const char *search, const char *replace) {
    char buffer[1024];
    char *insert_point = &buffer[0];
    const char *tmp = source;
    size_t len_search = strlen(search);
    size_t len_replace = strlen(replace);

    while (1) {
        const char *p = strstr(tmp, search);

        // Kein weiteres Vorkommen gefunden
        if (p == NULL) {
            strcpy(insert_point, tmp);
            break;
        }

        // Kopiere den Teil vor dem Vorkommen
        memcpy(insert_point, tmp, p - tmp);
        insert_point += p - tmp;

        // Kopiere die Ersetzung
        memcpy(insert_point, replace, len_replace);
        insert_point += len_replace;

        // Fortfahren nach dem Vorkommen
        tmp = p + len_search;
    }

    // Resultat kopieren
    strcpy(source, buffer);
}

int main() {
    char data[] = "Das ist ein Text mit einigen Worten. Diese Worten sind simpel.";
    searchAndReplace(data, "Worten", "Wörtern");

    printf("Ersetzter Text: %s\n", data);
    return 0;
}
```

Sample Output:

```
Ersetzter Text: Das ist ein Text mit einigen Wörtern. Diese Wörtern sind simpel.
```

## Deep Dive / Tieftauchen
Suchen und Ersetzen von Text in C ohne Standardbibliotheken war eine Herausforderung. Frühe C-Versionen boten nicht viel Unterstützung dafür. Heute nutzen wir `strstr` zum Suchen und `strcpy`, sowie `memcpy` zum Manipulieren von Strings.

Alternativen? Bibliotheken wie `regex.h` bieten reguläre Ausdrücke für komplexe Suchmuster. Andere Sprachen bieten hier oft mehr, zum Beispiel Python mit `str.replace`.

Die Implementierung oben arbeitet in-place, um den Speicherbedarf zu minimieren. Größere Texte oder komplexere Muster erfordern jedoch oft robustere Lösungen mit dynamischem Speicher.

## See Also / Siehe Auch
Weiterführende Links für neugierige Leser:

- [C Standard Library](https://en.cppreference.com/w/c/header)
- [C Dynamic Memory Allocation](https://en.cppreference.com/w/c/memory)
- [POSIX regex.h](https://pubs.opengroup.org/onlinepubs/7908799/xsh/regex.h.html)