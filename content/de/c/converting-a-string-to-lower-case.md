---
title:    "C: Umwandlung eines Strings in Kleinbuchstaben"
keywords: ["C"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/de/c/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Warum

Viele Programmierer stehen oft vor der Aufgabe, einen String in Kleinbuchstaben umzuwandeln. Dies kann aus verschiedenen Gründen notwendig sein, beispielsweise um Benutzereingaben zu vereinheitlichen oder bei der Verarbeitung von Texten. In diesem Blog-Beitrag werden wir uns anschauen, wie man einen String in C Programmierung in Kleinbuchstaben umwandeln kann.

## Wie geht man vor?

Um einen String in C in Kleinbuchstaben umzuwandeln, gibt es verschiedene Herangehensweisen. Eine mögliche Lösung wäre, den ASCII-Wert jedes einzelnen Buchstabens im String zu überprüfen und gegebenenfalls um 32 zu reduzieren, um den Großbuchstaben in einen Kleinbuchstaben umzuwandeln. Hier ist ein Beispielcode mit der Verwendung von `tolower()` Funktion:

```C
#include <stdio.h>
#include <ctype.h>

int main() {
  char str[] = "Hallo Welt!";
 
  for (int i = 0; str[i] != '\0'; i++) {
    str[i] = tolower(str[i]);
  }
 
  printf("Kleinbuchstaben: %s", str);
  return 0;
}

// Ausgabe: kleinbuchstaben: hallo welt!
```

In diesem Beispiel nutzen wir eine `for`-Schleife, um jeden Buchstaben im String zu durchlaufen. Mit `tolower()` wird dann jeder Buchstabe in einen Kleinbuchstaben umgewandelt. Am Ende wird der neue String mit der Funktion `printf()` ausgegeben.

Eine andere Möglichkeit ist die Verwendung der `strcpy()` Funktion, um den ursprünglichen String zu kopieren und dann mit `strlwr()` Funktion alle Buchstaben in Kleinbuchstaben umzuwandeln.

```C
#include <stdio.h>
#include <string.h>
#include <ctype.h>

int main() {
  char str1[] = "Hallo Welt!";
  char str2[100];
  
  strcpy(str2, str1); // kopiert den Inhalt von str1 in str2
  
  strlwr(str2); // wandelt alle Buchstaben in Kleinbuchstaben um
  
  printf("Kleinbuchstaben: %s", str2);
  return 0;
}

// Ausgabe: kleinbuchstaben: hallo welt!
```

Beide Methoden liefern das gleiche Ergebnis, die Wahl hängt also davon ab, welche mehr Lesbarkeit und Effizienz für den spezifischen Fall bietet.

## Tiefere Insights

Bei der ersten Methode wird direkt auf den ASCII-Wert des Buchstabens zugegriffen, was insgesamt effizienter ist. Bei der zweiten Methode wird jedoch erst eine Kopie des Strings erstellt, was zusätzliche Ressourcen benötigt. Es ist daher wichtig, die Vor- und Nachteile beider Methoden abzuwägen und diejenige zu wählen, die für Ihre spezifische Aufgabe am besten geeignet ist.

Zusätzlich kann es bei der Verwendung von `tolower()` zu Problemen mit Sonderzeichen und nicht-englischen Buchstaben kommen, da diese nicht immer die entsprechenden ASCII-Werte haben. In solchen Fällen sollten alternative Lösungen in Betracht gezogen werden.

## Siehe auch

- [C String-Bibliothek](https://www.tutorialspoint.com/c_standard_library/string_h.htm)
- [ASCII-Code Tabelle](https://www.cs.cmu.edu/~pattis/15-1XX/common/handouts/ascii.html)
- [ASCII vs. Unicode](https://www.computerhope.com/jargon/a/ascii.html)