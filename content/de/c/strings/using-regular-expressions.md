---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 18:10:48.252436-07:00
description: "Regul\xE4re Ausdr\xFCcke (regex) bieten eine M\xF6glichkeit, Zeichenfolgen\
  \ anhand definierter Muster zu suchen, zu vergleichen und zu manipulieren. Programmierer\u2026"
lastmod: '2024-03-13T22:44:54.341471-06:00'
model: gpt-4-0125-preview
summary: "Regul\xE4re Ausdr\xFCcke (regex) bieten eine M\xF6glichkeit, Zeichenfolgen\
  \ anhand definierter Muster zu suchen, zu vergleichen und zu manipulieren. Programmierer\u2026"
title: "Regul\xE4re Ausdr\xFCcke verwenden"
---

{{< edit_this_page >}}

## Was & Warum?

Reguläre Ausdrücke (regex) bieten eine Möglichkeit, Zeichenfolgen anhand definierter Muster zu suchen, zu vergleichen und zu manipulieren. Programmierer verwenden sie intensiv für Aufgaben wie die Validierung von Eingaben, das Parsen von Textdaten und das Finden von Mustern innerhalb großer Textdateien, was sie zu einem mächtigen Werkzeug in jeder Sprache macht, einschließlich C.

## Wie:

Um reguläre Ausdrücke in C zu verwenden, arbeiten Sie hauptsächlich mit der POSIX-Regex-Bibliothek (`<regex.h>`). Dieses Beispiel demonstriert einfaches Musterabgleichen:

```c
#include <stdio.h>
#include <stdlib.h>
#include <regex.h>

int main(){
    regex_t regex;
    int return_value;
    char *pattern = "^a[[:alnum:]]"; // Muster um Zeichenketten zu finden, die mit 'a' beginnen gefolgt von alphanumerischen Zeichen
    char *test_string = "apple123";

    // Den regulären Ausdruck kompilieren
    return_value = regcomp(&regex, pattern, REG_EXTENDED);
    if (return_value) {
        printf("Konnte regex nicht kompilieren\n");
        exit(1);
    }

    // Den regulären Ausdruck ausführen
    return_value = regexec(&regex, test_string, 0, NULL, 0);
    if (!return_value) {
        printf("Übereinstimmung gefunden\n");
    } else if (return_value == REG_NOMATCH) {
        printf("Keine Übereinstimmung gefunden\n");
    } else {
        printf("Regex-Abgleich fehlgeschlagen\n");
        exit(1);
    }

    // Speicher freigeben, der von dem regex verwendet wird
    regfree(&regex);

    return 0;
}
```

Beispielausgabe für eine übereinstimmende Zeichenfolge ("apple123"):
```
Übereinstimmung gefunden
```
Und für eine nicht übereinstimmende Zeichenfolge ("banana"):
```
Keine Übereinstimmung gefunden
```

## Tiefer eintauchen:

Reguläre Ausdrücke in C, als Teil des POSIX-Standards, bieten eine robuste Möglichkeit, Zeichenkettenabgleich und -manipulation durchzuführen. Allerdings wird die API der POSIX-Regex-Bibliothek in C im Vergleich zu den in Sprachen mit erstklassigen Zeichenkettenmanipulationsfunktionen wie Python oder Perl gefundenen APIs als umständlicher betrachtet. Die Syntax für Muster ist zwar sprachübergreifend ähnlich, jedoch erfordert C manuelle Speicherverwaltung und mehr Boilerplate-Code, um reguläre Ausdrücke vorzubereiten, auszuführen und danach aufzuräumen.

Trotz dieser Herausforderungen ist das Erlernen der Verwendung von regex in C lohnend, da es das Verständnis für Konzepte der Programmierung auf niedriger Ebene vertieft. Zusätzlich eröffnet es Möglichkeiten für die C-Programmierung in Bereichen wie Textverarbeitung und Datenextraktion, wo regex unverzichtbar ist. Für komplexere Muster oder Regex-Operationen könnte die PCRE-Bibliothek (Perl Compatible Regular Expressions) eine funktionsreichere und etwas einfachere Schnittstelle bieten, obwohl dies die Integration einer externen Bibliothek in Ihr C-Projekt erfordert.
