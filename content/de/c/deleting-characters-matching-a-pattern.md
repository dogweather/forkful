---
title:                "C: Entfernen von Zeichen, die einem Muster entsprechen"
simple_title:         "Entfernen von Zeichen, die einem Muster entsprechen"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/c/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

# Warum

Das Löschen von Zeichen, die einem bestimmten Muster entsprechen, ist eine gängige Aufgabe in der Programmierung. Es kann hilfreich sein, unerwünschte Zeichen oder Formatierungen aus einer Zeichenkette zu entfernen, um die Daten weiter zu verarbeiten oder anzuzeigen.

# Wie macht man das?

```C
#include <stdio.h>
#include <string.h>

int main() {
    char str[] = "Hallo, Welt!";
    char pattern[] = ","; // Das Muster, das gelöscht werden soll
    
    printf("Original: %s \n", str); // Gibt die ursprüngliche Zeichenkette aus
    
    // Durchläuft die Zeichenkette und entfernt alle Zeichen, die dem Muster entsprechen
    int index = 0;
    while (str[index]) {
        if (str[index] == pattern[0]) { // Vergleicht das aktuelle Zeichen mit dem ersten Zeichen des Musters
            memmove(&str[index], &str[index + 1], strlen(str) - index); // Verschiebt alle Zeichen nach dem aktuellen um eine Position nach vorne
        } else {
            index++;
        }
    }
    
    printf("Nach Entfernung des Musters: %s", str); // Gibt die neue Zeichenkette aus
    return 0;
}
```
**Output:**
```
Original: Hallo, Welt!
Nach Entfernung des Musters: Hallo Welt!
```

# Tiefergehende Informationen

Bei der Durchführung dieses Vorgangs gibt es einige wichtige Dinge zu beachten. Zum Beispiel muss man sicherstellen, dass genügend Speicherplatz vorhanden ist, um die Zeichenkette nach dem Löschen der Zeichen zu speichern. Auch sollte darauf geachtet werden, dass das Muster nicht aus mehreren Zeichen besteht, da dies beim Vergleich zu Fehlern führen kann.

Es gibt verschiedene Möglichkeiten, um das Entfernen von Zeichen auf eine effizientere Art und Weise zu implementieren, je nach Programmierumgebung und Anforderungen. Es kann auch nützlich sein, die Zeichenkette in ein anderes, speziell für die Bearbeitung von Zeichen optimiertes Format zu konvertieren.

# Siehe auch

- [String Manipulation in C](https://www.geeksforgeeks.org/string-manipulations-in-c-with-examples/)
- [String Functions in C](https://www.tutorialspoint.com/c_standard_library/c_function_strstr.htm)
- [Memmove Funktion in C](https://www.techonthenet.com/c_language/standard_library_functions/string_h/memmove.php)