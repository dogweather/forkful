---
title:                "Erzeugen von Zufallszahlen"
html_title:           "C: Erzeugen von Zufallszahlen"
simple_title:         "Erzeugen von Zufallszahlen"
programming_language: "C"
category:             "C"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/c/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Wie & Warum?
Generieren von Zufallszahlen ist eine Methode in der Programmierung, um numerische Werte zu erstellen, die zufällig und ohne Muster sind. Programmierer nutzen dies, um unvorhersehbare Variablen in ihren Programmen zu haben, z.B. bei der Erstellung von Spielen oder bei der Verschlüsselung von Daten.

## Wie geht es:
Es gibt verschiedene Möglichkeiten, um in C Zufallszahlen zu generieren. Eine beliebte Methode ist die Verwendung der Funktion `rand()`, die in der Standardbibliothek von C enthalten ist. Hier ist ein Beispielcode, der fünf zufällige Zahlen zwischen 1 und 10 generiert:

```c
#include <stdio.h>
#include <stdlib.h> // This library contains the function rand()

int main()
{
  int i;
  for(i = 0; i < 5; i++) // This loop will generate 5 random numbers
  {
    printf("%d\n", rand() % 10 + 1); // Prints a random number between 1 and 10
  }
  return 0;
}
```

Dies ist nur ein einfaches Beispiel, es gibt jedoch viele weitere Möglichkeiten, Zufallszahlen in C zu generieren. Sie können auch zufällige Buchstaben oder Zeichen mit entsprechenden Funktionen wie `rand() % 26 + 'a'` generieren.

## Tief tauchen:
Das Konzept der Zufallszahlen in der Programmierung hat eine lange Geschichte, die bis ins 19. Jahrhundert zurückreicht. Während der Entwicklung des Fortran-Programmiersprache wurde die Funktion `rand()` eingeführt, die die Grundlage für die meisten Zufallszahlengeneratoren in modernen Sprachen bildet. Es gibt auch alternative Methoden wie sogenannte "True Random Number Generators" (TNGRs), die auf zufälligen physikalischen Prozessen basieren und daher als sicherer erachtet werden.

Bei der Generierung von Zufallszahlen ist es wichtig, dass die Ergebnisse nicht vorhersehbar sind, da sie sonst gefälscht oder manipuliert werden können. Daher verwenden viele Programmierer komplexe Algorithmen und Techniken, um die Zufälligkeit zu erhöhen.

## Siehe auch:
- Offizielle C Dokumentation für `rand()`: https://en.cppreference.com/w/c/numeric/random/rand
- Blogpost über verschiedene Methoden zur Generierung von Zufallszahlen in C: https://rosettacode.org/wiki/Random_numbers#C