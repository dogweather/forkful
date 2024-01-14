---
title:                "C: Unterstrings extrahieren"
simple_title:         "Unterstrings extrahieren"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/c/extracting-substrings.md"
---

{{< edit_this_page >}}

## Warum

Das Extrahieren von Teilstrings ist eine nützliche Fähigkeit, die in vielen Fällen in der Programmierung benötigt wird. Es ermöglicht uns, Teile eines Textes zu isolieren, um sie später weiterzuverarbeiten oder zu analysieren.

## Wie man es macht

Um Teilstrings in C zu extrahieren, können wir die Funktion `strncpy` verwenden. Diese Funktion kopiert einen angegebenen Teil eines Strings in einen anderen String und gibt das Ergebnis zurück. Im folgenden Beispiel extrahieren wir einen Teilstring aus einem Namen und geben ihn aus:

```C
#include <stdio.h>

int main(void) {
  // Deklaration des ursprünglichen Strings
  char name[] = "Marie Müller";
  // Ausgabe des 2. bis 6. Zeichens des Strings
  char extracted[5];
  strncpy(extracted, name + 1, 5); // kopiert "arie "
  extracted[5] = '\0'; // fügt das Ende des Strings hinzu
  printf("Extrahierter Teilstring: %s\n", extracted);
  return 0;
}
```
Die Ausgabe wäre: `Extrahierter Teilstring: arie `. In diesem Beispiel wird der Teilstring "arie " aus dem ursprünglichen String "Marie Müller" extrahiert und in einem neuen String gespeichert.

## Tieferer Einblick

Die `strncpy` Funktion akzeptiert mehrere Parameter, um verschiedene Operationen auszuführen. Zum Beispiel können wir durch Manipulation der Startposition und der Länge des extrahierten Teils verschiedene Teilstrings erhalten. Es ist auch wichtig, darauf zu achten, den extrahierten String richtig zu terminieren, indem wir ein Nullzeichen am Ende hinzufügen.

Es gibt auch alternative Methoden zum Extrahieren von Teilstrings in C, wie zum Beispiel das Verwenden der `substr` Funktion oder das Manuellzählen der Zeichen. Indem Sie verschiedene Methoden kennenlernen, können Sie ihre Vor- und Nachteile verstehen und die für Ihre spezifischen Bedürfnisse am besten geeignete auswählen.

## Siehe auch

- [Die offizielle C-Dokumentation zur strncpy-Funktion](https://www.cplusplus.com/reference/cstring/strncpy/)
- [Ein Tutorial zur Verwendung der substr-Funktion in C](https://www.tutorialspoint.com/c_standard_library/c_function_substr.htm)
- [Eine Erklärung, wie man Teilstrings in C manuell extrahiert](https://www.geeksforgeeks.org/c-program-extracting-characters-from-a-string/)