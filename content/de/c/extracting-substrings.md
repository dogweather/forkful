---
title:                "Teilstrings extrahieren"
date:                  2024-01-20T17:44:59.324622-07:00
model:                 gpt-4-1106-preview
simple_title:         "Teilstrings extrahieren"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/c/extracting-substrings.md"
---

{{< edit_this_page >}}

## Was & Warum?
Das Extrahieren von Teilstrings bedeutet, dass wir ein Stück aus einem längeren String herausschneiden. Programmierer nutzen das, um spezifische Daten zu verarbeiten, Informationen zu validieren oder Formatierungen anzupassen.

## Wie geht das?
```C
#include <stdio.h>
#include <string.h>

int main() {
    char text[] = "Wir lernen C-Programmierung!";
    char substr[20];

    // Extrahieren eines Teilstrings ab Position 4, Länge 7
    strncpy(substr, text + 4, 7);
    substr[7] = '\0'; // Null-Terminator nicht vergessen!
    
    printf("Teilstring: %s\n", substr);
    return 0;
}
```
Ausgabe:
```
Teilstring: lernen
```

## Tiefere Einblicke
Teilstrings zu extrahieren, ist so alt wie das Programmieren mit Strings selbst. C bietet keine eingebaute Substring-Funktion, anders als höhere Sprachen wie Python oder Java. `strncpy()` ist jedoch ein Standardwerkzeug, und mit Kenntnis von Pointern lässt sich viel erreichen. Man sollte vorsichtig sein, um Pufferüberläufe und fehlende Null-Terminatoren zu vermeiden. Alternativen sind Bibliotheken wie `strstr` zum Finden von Substrings oder das manuelle Iterieren durch den String.

## Siehe auch
- C Standard Library documentation on `strncpy()`: https://en.cppreference.com/w/c/string/byte/strncpy
- Tutorial über Pointer in C: https://www.tutorialspoint.com/cprogramming/c_pointers.htm
- "The C Programming Language" von K&R - klassische Lektüre zum Umgang mit Strings und Pointern.