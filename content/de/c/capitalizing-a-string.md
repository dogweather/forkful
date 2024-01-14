---
title:                "C: String in Großbuchstaben umwandeln"
programming_language: "C"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/c/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Warum
Es gibt viele Gründe, warum man in der C-Programmierung eine Zeichenkette großschreiben möchte. Vielleicht müssen bestimmte Wörter oder Sätze in Großbuchstaben angezeigt werden, um sie besser hervorzuheben. Oder es könnte Teil einer Validierungsprozedur sein, um sicherzustellen, dass bestimmte Eingaben immer in Großbuchstaben sind. Egal aus welchem Grund, das Großschreiben einer Zeichenkette ist eine nützliche Fähigkeit in der C-Programmierung.

## Wie geht das
Es gibt verschiedene Möglichkeiten, eine Zeichenkette in C groß zu schreiben. Eine Möglichkeit ist die Verwendung der Standard-C-Bibliotheksfunktion `toupper()`, die ein einzelnes Zeichen in einen Großbuchstaben umwandelt. Hier ist ein Beispielcode:

```C
#include <stdio.h>
#include <ctype.h>

int main() {
   char string[] = "Hallo Welt";
   int i;

   for(i = 0; string[i] != '\0'; i++) {
      string[i] = toupper(string[i]);
   }

   printf("Die zeichenkette in Großbuchstaben lautet: %s", string);
   return 0;
}
```

Die Ausgabe dieses Codes sollte die Zeichenkette "HALLO WELT" sein.

Eine andere Möglichkeit ist die Verwendung von Schleifen und Bedingungen, um jeden Buchstaben einzeln zu überprüfen und gegebenenfalls in einen Großbuchstaben umzuwandeln. Dies ist etwas komplexer, aber ermöglicht mehr Kontrolle und Anpassungsmöglichkeiten für spezielle Fälle.

## Tiefere Einblicke
Die `toupper()` Funktion akzeptiert nur einen einzelnen Buchstaben als Argument und gibt den entsprechenden Großbuchstaben zurück. Wenn Sie jedoch eine ganze Zeichenkette in Großbuchstaben umwandeln möchten, müssen Sie sie in einer Schleife durchlaufen und jeden Buchstaben einzeln umwandeln. Es gibt auch eine ähnliche Funktion namens `tolower()` für die Umwandlung in Kleinbuchstaben.

Es ist wichtig zu beachten, dass die Funktionen `toupper()` und `tolower()` nur für Buchstaben des ASCII-Zeichensatzes funktionieren. Wenn Sie Zeichen einer anderen Sprache verwenden, z.B. mit Akzenten oder Umlauten, müssen Sie möglicherweise eine andere Methode verwenden.

## Siehe auch
- [String Funktionen in C](https://www.tutorialspoint.com/c_standard_library/c_function_string_h.htm)
- [ASCII-Code-Tabelle](https://www.ascii-code.com/)