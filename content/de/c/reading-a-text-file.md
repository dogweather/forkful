---
title:                "Textdatei einlesen"
date:                  2024-01-20T17:53:54.939196-07:00
model:                 gpt-4-1106-preview
simple_title:         "Textdatei einlesen"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/c/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Was & Warum?
Das Einlesen einer Textdatei bedeutet, deren Inhalt in ein Programm zu laden. Das machen Programmierer, um Daten zu verarbeiten oder Konfigurationen zu laden.

## How to:
Im Folgenden findet ihr ein einfaches Beispiel, wie eine Textdatei zeilenweise gelesen wird:

```C
#include <stdio.h>
#include <stdlib.h>

int main() {
    FILE *datei = fopen("beispiel.txt", "r"); // Datei zum Lesen öffnen
    if (datei == NULL) {
        perror("Fehler beim Öffnen der Datei");
        return EXIT_FAILURE;
    }

    char zeile[256];
    while (fgets(zeile, sizeof(zeile), datei)) {
        printf("%s", zeile);
    }

    fclose(datei); // Datei schließen
    return EXIT_SUCCESS;
}
```

Angenommen, `beispiel.txt` enthält:
```
Hallo Welt!
Das ist eine Textdatei.
```

Die Ausgabe des Programms wäre:
```
Hallo Welt!
Das ist eine Textdatei.
```

## Deep Dive
Das Lesen von Textdateien ist eine grundlegende Operation, die seit den frühen Tagen der Programmierung besteht. Frühere Methoden umfassten das direkte Lesen aus dem Dateisystem ohne Standards wie `fopen`. Alternativen zum Lesen von Dateien in C sind die Verwendung von System- oder Bibliotheksfunktionen wie `read` (aus `unistd.h`) oder höheren Abstraktionen wie Streams aus C++.

Bei der Implementierung sollte beachtet werden, wie mit großen Dateien umgegangen wird. Der hier gezeigte Ansatz mit `fgets` ist für kleine bis mittelgroße Dateien ausreichend. Für große Dateien könnte man den Inhalt stückweise verarbeiten, um den Speicherverbrauch im Zaum zu halten.

Fehlerbehandlung ist auch wichtig. Wenn `fopen` scheitert, gibt es NULL zurück. Mithilfe von `perror` oder `strerror` kann man dann die Ursache herausfinden. Es ist auch notwendig, geöffnete Dateien mit `fclose` zu schließen, um Ressourcen-Leaks zu verhindern.

## See Also
- C Standard Library Dokumentation: https://en.cppreference.com/w/c/io
- Tutorial zum Einlesen von Dateien in C: https://www.cprogramming.com/tutorial/c-file-io.html
- Fehlerbehandlung in C: https://www.tutorialspoint.com/c_standard_library/error_handling_in_c.htm
