---
title:                "Ein String in Kleinbuchstaben umwandeln"
html_title:           "C: Ein String in Kleinbuchstaben umwandeln"
simple_title:         "Ein String in Kleinbuchstaben umwandeln"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/c/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Warum

Es mag dir vielleicht nicht wichtig erscheinen, aber das Umwandeln von Groß- zu Kleinbuchstaben ist eine häufige Aufgabe in der Programmierung. Zum Beispiel kann es notwendig sein, Benutzereingaben zu formatieren oder Vergleiche in Kontrollstrukturen durchzuführen. Daher ist es wichtig, zu verstehen, wie man das in C bewerkstelligt.

## Wie geht's

Um eine Zeichenkette in Kleinbuchstaben umzuwandeln, gibt es mehrere Möglichkeiten in C. Eine Option ist die Verwendung der Standardbibliotheksfunktion `tolower()`, die in `ctype.h` deklariert ist. Hier ist ein Beispiel:

```C
#include <stdio.h>
#include <ctype.h>

int main(void) {
  char word[] = "Programmieren";

  for (int i = 0; word[i] != '\0'; i++) {
    word[i] = tolower(word[i]);
  }

  printf("Kleinbuchstaben: %s\n", word);
  return 0;
}
```

Dieser Code nutzt eine `for`-Schleife, um jeden Buchstaben in der Zeichenkette `word` in einen Kleinbuchstaben zu konvertieren. Anschließend wird die konvertierte Zeichenkette mit der `printf()`-Funktion ausgegeben.

Eine andere Möglichkeit ist die Verwendung der Funktion `strlwr()`, die in der Bibliothek `string.h` deklariert ist. Hier ist ein Beispiel für die Verwendung:

```C
#include <stdio.h>
#include <string.h>

int main(void) {
  char word[] = "Programmieren";

  strlwr(word);

  printf("Kleinbuchstaben: %s\n", word);
  return 0;
}
```

Der Unterschied hier ist, dass die Umwandlung in Kleinbuchstaben mithilfe der Funktion `strlwr()` direkt auf die Zeichenkette `word` angewendet wird.

## Tiefergehende Infos

Es ist wichtig zu beachten, dass die Methode, die in diesem Artikel vorgestellt wird, nicht ohne ihre Einschränkungen ist. Zum Beispiel kann die `tolower()`-Methode nur für einzelne Zeichen angewendet werden und nicht auf ganze Zeichenketten. Außerdem kann sie möglicherweise nicht alle Zeichen korrekt in Kleinbuchstaben umwandeln, da sie für ASCII-Zeichen ausgelegt ist.

Für fortgeschrittenere Ansätze kann selbst eine Funktion geschrieben werden, die eine Zeichenkette in Kleinbuchstaben umwandelt. Dies würde es ermöglichen, Sonderzeichen, Unicode-Zeichen und andere Zeichenformate zu berücksichtigen. Es gibt auch Bibliotheken von Drittanbietern, die diese Aufgabe vereinfachen können.

## Siehe auch

- [ASCII-Tabelle](https://de.wikipedia.org/wiki/American_Standard_Code_for_Information_Interchange)
- [C Zeichenketten](https://www.tutorialspoint.com/cprogramming/c_strings.htm)
- [ctype.h Referenz](https://www.tutorialspoint.com/c_standard_library/ctype_h.htm)