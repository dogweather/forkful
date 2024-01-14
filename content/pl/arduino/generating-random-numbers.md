---
title:    "Arduino: Generowanie losowych liczb"
keywords: ["Arduino"]
---

{{< edit_this_page >}}

## Dlaczego

Generowanie losowych liczb jest funkcją wykorzystywaną w wielu projektach Arduino, takich jak gry planszowe, symulatory rzutu kośćmi lub generatory muzyki. Jest to również przydatne narzędzie do testowania i symulacji różnych scenariuszy w programie. W tym artykule dowiesz się, jak w prosty sposób wygenerować losowe liczby w Arduino.

## Jak to zrobić

Jednym z najbardziej popularnych sposobów generowania losowych liczb w Arduino jest użycie funkcji ```random()```. Ta funkcja zwraca losową liczbę całkowitą między 0 a 32767. Możemy również określić zakres, w którym ma zostać wygenerowana liczba, używając funkcji ```random(min, max)```, gdzie ```min``` jest najmniejszą możliwą liczbą, a ```max``` jest największą. Na przykład, jeśli chcielibyśmy wylosować liczbę między 1 a 6, możemy użyć kodu:

```Arduino
int randomNum = random(1, 6);
```

Możemy również użyć funkcji ```randomSeed()```, aby określić ziarno (seed) dla naszego generatora liczb losowych. Ziarno jest liczbą, która ma wpływ na wynik generowanej liczby. Jeśli użyjemy tej samej liczby ziarna, za każdym razem zostanie wygenerowana ta sama liczba. To przydatne, jeśli chcemy, aby nasz program był wykonywalny, ale generował wciąż inne liczby. Przykładowy kod poniżej:

```Arduino
randomSeed(42); //ustawia ziarno na 42
int randomNum = random(1, 6);
```

## Deep Dive

W Arduino jest wykorzystywany generator liczb pseudolosowych, co oznacza, że wynik jest generowany w sposób deterministyczny, ale wydaje się być losowy. Jest to związane z faktem, że komputer jest maszyną programowalną i może wykonać określone czynności tylko w określonym porządku, więc nie jest w stanie wygenerować prawdziwie losowych liczb.

Ponadto, aby być bardziej „losowym”, generator korzysta z określonego algorytmu do wyboru kolejnych liczb. W przypadku funkcji ```random()``` w Arduino, używany jest algorytm nazwany Linear Congruential Generator (LCG). Algorytm ten wykorzystuje pewne stałe liczbowe, aby wygenerować kolejne liczby w sposób, który ma wydawać się być całkowicie losowy.

## Zobacz także

- [Arduino Reference - random()](https://www.arduino.cc/reference/en/language/functions/random-numbers/random/)
- [Arduino Reference - randomSeed()](https://www.arduino.cc/reference/en/language/functions/random-numbers/randomseed/)
- [Explanation of Linear Congruential Generator](https://en.wikipedia.org/wiki/Linear_congruential_generator)