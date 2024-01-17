---
title:                "Generowanie losowych liczb"
html_title:           "Arduino: Generowanie losowych liczb"
simple_title:         "Generowanie losowych liczb"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/arduino/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Co i dlaczego?

Generowanie losowych liczb jest ważnym elementem programowania, ponieważ pozwala na tworzenie różnorodnych i nieprzewidywalnych wyników. Programiści często wykorzystują losowe liczby do symulacji zdarzeń, generowania unikalnych identyfikatorów i wielu innych zastosowań.

## Jak to zrobić:

```Arduino
// Losowanie liczby z przedziału 0-99
int liczba = random(100);

// Wyświetlenie wylosowanej liczby na serial monitorze
Serial.println(liczba);
```

**Output:**
```
39
```

## Wnikliwe zagłębienie:

### Kontekst historyczny:
Generowanie losowych liczb jest wykorzystywane już od dawna w informatyce i matematyce. Pierwszą metodą wykorzystywaną do tego celu była tzw. metoda liniowa kongruencji, opracowana w latach 40. XX wieku. Obecnie programiści mogą wybierać spośród wielu różnych algorytmów generujących losowe liczby.

### Alternatywy:
Arduino posiada wbudowaną funkcję `random()`, jednak można także skorzystać z biblioteki `randomSeed()` do wprowadzenia bardziej losowych danych w procesie generowania liczb. Istnieją również inne sposoby na generowanie liczb pseudolosowych, takie jak np. z wykorzystaniem zewnętrznego generatora losowych liczb (RNG).

### Szczegóły implementacyjne:
Funkcja `random()` w Arduino korzysta z algorytmu nazwanego "Linear Congruential Generator" (LCG), który jest jednym z najprostszych i najpopularniejszych sposobów na generowanie liczb pseudolosowych. Można także użyć innych algorytmów, takich jak np. "Mersenne Twister" lub "Xorshift".

## Zobacz także:

- Oficjalna dokumentacja Arduino dotycząca funkcji `random()`: https://www.arduino.cc/reference/en/language/functions/random-numbers/random/
- Poradnik na temat generowania liczb pseudolosowych w Arduino: https://randomnerdtutorials.com/guide-for-random-numbers-with-arduino-and-arduino-due/
- Wprowadzenie do różnych metod generowania liczb pseudolosowych: https://www.javatpoint.com/cpp-random-number