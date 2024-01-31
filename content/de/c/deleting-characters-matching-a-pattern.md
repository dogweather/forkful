---
title:                "Löschen von Zeichen, die einem Muster entsprechen"
date:                  2024-01-20T17:41:44.562919-07:00
model:                 gpt-4-1106-preview
simple_title:         "Löschen von Zeichen, die einem Muster entsprechen"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/c/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## What & Why? (Was & Warum?)
Das Löschen von Zeichen, die einem Muster entsprechen, bedeutet, bestimmte Zeichen aus einem Text zu entfernen. Programmierer nutzen diese Technik, um Eingaben zu säubern, Daten zu formatieren oder unerwünschte Information zu entfernen.

## How to: (Wie geht das:)
```C
#include <stdio.h>
#include <string.h>

void delete_pattern(char *str, const char *pattern) {
    char *src = str, *dst = str;
    while (*src) {
        const char *p = pattern;
        // Prüfen, ob aktuelles Zeichen ein Muster entspricht.
        while (*p && *src != *p) p++;
        
        // Wenn Zeichen nicht im Muster ist, kopieren.
        if (*p == '\0')
            *dst++ = *src;
        src++;
    }
    *dst = '\0';
}

int main() {
    char text[] = "Hallo Welt! 123";
    // Zeichen die gelöscht werden sollen: Leerzeichen und Ziffern
    delete_pattern(text, " 1234567890");
    printf("%s\n", text); // Ausgabe: "HalloWelt!"
    return 0;
}
```

## Deep Dive (Tieferer Einblick)
Früher, vor modernen hohen Programmiersprachen, war es üblich, Zeichenmanipulationen direkt auf niedriger Speicherebene durchzuführen. Dies beinhaltete direkte Pointeroperationen und oft manuelle Schleifen durch Strings.

Heutzutage gibt es viele Alternativen: Reguläre Ausdrücke und höherwertige String-Verarbeitungsfunktionen in Bibliotheken wie `regex.h` oder in Sprachen wie Python und JavaScript. 

Bei der obigen Implementierung gehen wir Zeichen für Zeichen durch und behalten nur jene, die nicht im Muster sind. Das ist sicher und vermeidet Schaffung neuer Strings, spart also Speicher.

## See Also (Siehe auch)
- C Standard Library: https://en.cppreference.com/w/c/string/byte
- Reguläre Ausdrücke in C: https://en.cppreference.com/w/c/regex
- Möglichkeiten der String-Manipulation: https://www.gnu.org/software/libc/manual/html_node/String-and-Array-Utilities.html
