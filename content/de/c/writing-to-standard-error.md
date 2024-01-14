---
title:    "C: Schreiben in den Standardfehler"
keywords: ["C"]
---

{{< edit_this_page >}}

## Warum

Das Schreiben von Informationen auf den Standardfehler (oder stderr) ist ein wichtiger Teil des Debuggings und der Fehlerbehebung in der Programmierung. Durch die Ausgabe von Fehlern, Warnungen und anderen wichtigen Informationen auf stderr können Entwickler schnell erkennen, was schief gelaufen ist und wo das Problem liegt.

## So geht's

Um auf stderr zu schreiben, muss zunächst die Standardbibliothek `stdio.h` eingebunden werden. Dann kann die Funktion `fprintf()` verwendet werden, um die gewünschten Informationen auf stderr auszugeben.

Beispielcode:

```C
#include <stdio.h>

int main() {
    int x = 10;

    if (x > 5) {
        fprintf(stderr, "x ist größer als 5\n");
    } else {
        fprintf(stderr, "x ist kleiner als 5\n");
    }

    return 0;
}
```

Sample Output:

```
x ist größer als 5
```

Dieses einfache Beispiel zeigt, wie **fprintf()** verwendet werden kann, um wichtige Informationen auf stderr auszugeben. Es ist wichtig zu beachten, dass stderr unabhängig von stdout ist, d.h. Ausgaben auf stderr werden nicht von der Standardeinschränkung `>` erfasst.

## Tiefere Einblicke

Das Schreiben auf stderr kann auch nützlich sein, um den Fortschritt oder bestimmte Ereignisse in einem Programm zu verfolgen. Es kann auch hilfreich sein, wenn ein Programm in einer Umgebung ohne Terminal oder Konsole ausgeführt wird, da die stderr-Ausgabe nicht an einen bestimmten Ausgabekanal gebunden ist.

Darüber hinaus kann das Schreiben auf stderr auch bei der Nutzung von Drittanbieter-Bibliotheken nützlich sein. Wenn eine Bibliothek eine Fehlermeldung auf stderr ausgibt, kann dies beim Debugging helfen und dem Entwickler dabei helfen, das Problem zu identifizieren und zu beheben.

## Siehe auch

- [Dokumentation von `fprintf()`](https://www.cplusplus.com/reference/cstdio/fprintf/)
- [Verwendung von stderr für Fehlersuchen in C](https://www.includehelp.com/c-programs/standard-error-stderr-in-c-programming.aspx)
- [Ein Vergleich von stdout und stderr in C](https://www.baeldung.com/linux/commands-stdout-stderr)