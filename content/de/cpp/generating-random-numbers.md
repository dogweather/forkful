---
title:                "Erzeugung zufälliger Zahlen"
html_title:           "C++: Erzeugung zufälliger Zahlen"
simple_title:         "Erzeugung zufälliger Zahlen"
programming_language: "C++"
category:             "C++"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/cpp/generating-random-numbers.md"
---

{{< edit_this_page >}}

# Was & Warum?
Generieren von Zufallszahlen ist eine Technik, die in der Programmierung häufig verwendet wird, um zufällige oder pseudo-zufällige Werte zu erzeugen. Dies kann hilfreich sein, um Simulationen zu erstellen, Daten zu generieren oder Spiele zu programmieren.

# Wie geht's?
Um in C++ Zufallszahlen zu generieren, muss die Bibliothek `cstdlib` eingebunden werden. Dann kann die Funktion `rand()` verwendet werden, um eine Zufallszahl zwischen 0 und `RAND_MAX` zu generieren. Um eine benutzerdefinierte Range zu haben, kann die Funktion `srand()` verwendet werden, um den Startpunkt festzulegen. Hier ist ein Beispiel:

```C++
#include <cstdlib>
#include <iostream>

int main()
{
  // Eine Zufallszahl in der Range von 1 bis 10 erzeugen
  srand(10); // Startpunkt auf 10 setzen
  int random_number = rand() % 10 + 1; // Ein zufälliger Wert zwischen 0 und 9 wird generiert und mit 1 addiert, um die Range 1 bis 10 zu erhalten.
  std::cout << random_number << "\n"; // Beispiel: 7

  // Eine Zufallszahl in der Range von 50 bis 100 erzeugen
  srand(100); // Startpunkt auf 100 setzen
  random_number = rand() % 51 + 50; // Ein zufälliger Wert zwischen 0 und 50 wird generiert und mit 50 addiert, um die Range 50 bis 100 zu erhalten.
  std::cout << random_number << "\n"; // Beispiel: 78
  
  return 0;
}
```

# Tiefgründig
Das Generieren von Zufallszahlen ist eine Technik, die bereits seit den Anfängen der Computerprogrammierung verwendet wird. Die Funktion `rand()` basiert auf einem Algorithmus, der ein mathematisches Konzept namens linearer Kongruenzgenerator verwendet. Dieser Algorithmus ist nicht perfekt und kann gewisse Muster aufweisen, weshalb alternative Methoden wie die Verwendung von externen Zufallszahlengeneratoren empfohlen werden.

# Siehe auch
- [C++ Referenz für die Funktion `rand()`](https://www.cplusplus.com/reference/cstdlib/rand/)
- [Linearer Kongruenzgenerator](https://de.wikipedia.org/wiki/Linearer_Kongruenzgenerator)
- [Externe Zufallszahlengeneratoren](https://de.wikipedia.org/wiki/Externer_Zufallszahlengenerator)