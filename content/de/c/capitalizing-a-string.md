---
title:                "Großschreibung einer Zeichenkette"
html_title:           "C: Großschreibung einer Zeichenkette"
simple_title:         "Großschreibung einer Zeichenkette"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/c/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Warum

Strings sind eine wichtige Datenstruktur in C, die oft verwendet werden, um Text in Programmen zu speichern. Das Kapitalisieren eines Strings kann hilfreich sein, um die Lesbarkeit zu verbessern oder bestimmte Anforderungen an den Benutzer eingehalten werden müssen.

## Wie geht man vor

Das Kapitalisieren eines Strings kann auf verschiedene Weise erfolgen, je nach den spezifischen Anforderungen des Programms. Im Folgenden werden zwei Beispiele gegeben:

```C
// Beispiel 1: Kapitalisierung des gesamten Strings
#include <stdio.h>
#include <ctype.h>

int main() {
    char string[50];
    
    printf("Geben Sie einen String ein: ");
    fgets(string, 50, stdin); // liest input von der Konsole
    
    int i = 0;
    while (string[i] != '\0') {
        string[i] = toupper(string[i]); // wandelt jeden Buchstaben in Großbuchstaben um
        i++;
    }
    
    printf("Kapitalisierter String: %s", string);
    
    return 0;
}
```

```C
// Beispiel 2: Kapitalisierung der ersten Buchstaben in jedem Wort
#include <stdio.h>
#include <string.h>
#include <ctype.h>

int main() {
    char string[50];
    
    printf("Geben Sie einen String ein: ");
    fgets(string, 50, stdin);
    
    int i = 0;
    while (string[i] != '\0') {
        string[i] = toupper(string[i]); // wandelt ersten Buchstaben in Großbuchstaben um
        // überprüft, ob vorheriges Zeichen ein Leerzeichen oder Tabulator war
        // und wandelt anschließend den nächsten Buchstaben in Großbuchstaben um
        if (string[i] == ' ' || string[i] == '\t') {
            string[i + 1] = toupper(string[i + 1]);
        }
        i++;
    }
    
    printf("Kapitalisierter String: %s", string);
    
    return 0;
}
```

### Tiefergehende Erklärung

Die Funktion `toupper()` aus der Bibliothek <ctype.h> wandelt einen einzelnen Buchstaben in Großbuchstaben um. Dazu wird der ASCII-Wert des Buchstabens verwendet. Bei Kleinbuchstaben muss der ASCII-Wert um 32 verringert werden, um den entsprechenden Großbuchstaben zu erhalten.

Im ersten Beispiel wird in einer Schleife jeder Buchstabe der Reihe nach mit der `toupper()`-Funktion umgewandelt. Dadurch wird der gesamte String in Großbuchstaben geschrieben.

Im zweiten Beispiel wird zusätzlich überprüft, ob der vorherige Buchstabe ein Leerzeichen oder Tabulator war. Wenn dies der Fall ist, wird der Buchstabe dahinter in Großbuchstaben umgewandelt. Dadurch entsteht ein Satz mit großgeschriebenen Anfangsbuchstaben in jedem Wort.

## Weitere Informationen

- [ASCII-Tabelle](https://www.asciitable.com/)
- [ctype.h-Dokumentation](https://www.tutorialspoint.com/c_standard_library/ctype_h.htm)

## Siehe auch

- [String Operations in C](https://www.geeksforgeeks.org/string-handling-operations-in-c/)
- [Looping in C](https://www.programiz.com/c-programming/c-for-loop)