---
title:                "Generowanie losowych liczb."
html_title:           "Arduino: Generowanie losowych liczb."
simple_title:         "Generowanie losowych liczb."
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/arduino/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Dlaczego
Dlaczego generowanie losowych liczb jest ważne w programowaniu? Wiele projektów wymaga wykorzystania przypadkowych wartości, takich jak gry planszowe, symulacje i generowanie haseł.

## Jak to zrobić
Aby wygenerować losową liczbę w Arduino, skorzystaj z funkcji ```random()```. Możesz określić zakres liczb, z jakiego ma zostać wybrana wartość, np. ```random(1, 10)``` zwróci losową liczbę z przedziału od 1 do 10.

```Arduino
int randomNumber = random(1, 10); // generuje losową liczbę w zakresie od 1 do 10
Serial.println(randomNumber); // wyświetla liczbę na monitorze szeregowym
```

W przypadku generowania liczb zmiennoprzecinkowych, możesz użyć funkcji ```randomFloat()```. Przykładowy kod:

```Arduino
float randomFloat = randomFloat(0.5, 1.5); // generuje losową liczbę zmiennoprzecinkową z przedziału od 0.5 do 1.5
Serial.println(randomFloat); // wyświetla liczbę na monitorze szeregowym
```

Możesz również użyć funkcji ```randomSeed()``` do ustawienia ziarna generacji, co pozwala na uzyskanie różnych sekwencji liczb w każdym uruchomieniu programu. Przykładowy kod:

```Arduino
int seed = 123; // ustawia ziarno na 123
randomSeed(seed); // ustawia ziarno generacji
int randomNumber = random(1, 100); // generuje losową liczbę w zakresie od 1 do 100
Serial.println(randomNumber); // wyświetla liczbę na monitorze szeregowym
```

## Deep Dive
Funkcja ```random()``` w Arduino wykorzystuje generator liczb pseudolosowych typu LCG (ang. Linear Congruential Generator). Działanie tego generatora opiera się na mnożeniu, dodawaniu i odanie modulo liczby n, co pozwala na wygenerowanie sekwencji liczb, które wydają się być losowe. Należy jednak pamiętać, że liczby generowane przez ten algorytm są w rzeczywistości deterministyczne i mogą powtarzać się po pewnym czasie.

Jeśli potrzebujesz bardziej zaawansowanej metody generowania losowych liczb, możesz skorzystać z biblioteki ```random()``` lub zainstalować dodatkową bibliotekę, taką jak ```Entropy```, która wykorzystuje losowe fluktuacje sygnału analogowego do generowania bardziej losowych danych.

## Zobacz też
- [Dokumentacja Arduino - random()](https://www.arduino.cc/reference/en/language/functions/random-numbers/random/)
- [Biblioteka RandomSeed](https://www.arduino.cc/reference/en/libraries/random/)
- [Biblioteka Entropy](https://github.com/aaronds/arduino-entropy)