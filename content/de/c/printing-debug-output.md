---
title:                "Debug-Ausgaben drucken"
date:                  2024-01-20T17:52:02.422019-07:00
model:                 gpt-4-1106-preview
simple_title:         "Debug-Ausgaben drucken"
programming_language: "C"
category:             "C"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/c/printing-debug-output.md"
---

{{< edit_this_page >}}

## Was & Warum?
Drucken von Debug-Informationen ist das Anzeigen von Zwischenwerten und Prozessinformationen während der Laufzeit eines Programms. Programmierer nutzen es, um Fehler zu finden und das Verständnis der Codeabläufe zu verbessern.

## So geht's:
Verwenden wir `printf` zum Debuggen. Es zeigt Daten auf der Standardausgabe an.

```C
#include <stdio.h>

int main() {
    int loopVar = 0;
    for(loopVar = 0; loopVar < 5; loopVar++) {
        printf("Loop Iteration: %d\n", loopVar);
    }
    // Stellen Sie sich vor, wir haben einen Fehler hier
    printf("Der Wert von loopVar sollte 5 sein: %d\n", loopVar);
    return 0;
}
```

Ausgabe:

```
Loop Iteration: 0
Loop Iteration: 1
Loop Iteration: 2
Loop Iteration: 3
Loop Iteration: 4
Der Wert von loopVar sollte 5 sein: 5
```

## Tiefgang:
Früher gab's keine IDEs; Textausgaben waren grundlegend fürs Debugging. Heute gibt es Alternativen wie integrierte Debugger oder Logging-Bibliotheken, die mehr Kontrolle und Flexibilität bieten. Die `printf`-Funktion kommt aus der C-Standardbibliothek und schreibt auf `stdout`. Umleiten dieser Ausgabe ist möglich und oft in komplexeren Umgebungen nützlich.

## Siehe auch:
- GNU Debugger (GDB): https://www.gnu.org/software/gdb/
- Logging-Bibliotheken: https://github.com/gabime/spdlog
- C Standard Library Reference (stdio.h): https://en.cppreference.com/w/c/io
