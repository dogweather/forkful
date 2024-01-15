---
title:                "Generowanie losowych liczb"
html_title:           "Java: Generowanie losowych liczb"
simple_title:         "Generowanie losowych liczb"
programming_language: "Java"
category:             "Java"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/java/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Dlaczego

Generowanie liczb losowych jest niezbędnym elementem wielu aplikacji, takich jak gry czy symulacje. Losowość daje możliwość tworzenia różnorodnych scenariuszy i zapewnia ciekawą rozgrywkę. Przeczytaj dalej, aby dowiedzieć się jak w prosty sposób wygenerować losowe liczby w języku Java.

## Jak to zrobić

Krok 1: Użyj klasy Random do generowania liczb losowych.
```Java
Random random = new Random();
```

Krok 2: Użyj metody nextInt() aby wygenerować losową liczbę całkowitą. Możesz również określić zakres, w którym ta liczba ma być wygenerowana, podając dwie liczby całkowite jako argumenty metody.
```Java
int randomNumber = random.nextInt(); // wygeneruje losową liczbę całkowitą
int rangeRandomNumber = random.nextInt(50); // wygeneruje liczbę całkowitą z zakresu od 0 do 50
```

Krok 3: Użyj metody nextDouble() aby wygenerować losową liczbę zmiennoprzecinkową.
```Java
double randomDouble = random.nextDouble(); // wygeneruje losową liczbę zmiennoprzecinkową
```

Możesz również wygenerować losową liczbę zmiennoprzecinkową o określonej wartości minimalnej i maksymalnej, wykorzystując poniższą formułę:
```Java
double rangeRandomDouble = random.nextDouble() * (max - min) + min;
```

## Deep Dive

Klasa Random wykorzystuje algorytm nazywany "Linear Congruential Generator", który generuje liczby w sposób deterministyczny. Oznacza to, że jeśli podasz ten sam ziarno (seed) do obiektu klasy Random, to otrzymasz takie same wyniki.

Jeśli chcesz uzyskać różne wyniki z wykorzystaniem tego samego obiektu klasy Random, możesz zmienić ziarno przez użycie metody setSeed(). Możesz również stworzyć nowy obiekt klasy Random z różnym ziarnem.

Klasa Random oferuje także inne metody do generowania różnego rodzaju liczb, takich jak nextLong() czy nextBoolean(). Zalecamy przeczytanie dokumentacji Javy, aby uzyskać więcej informacji o tych metodach i algorytmach generujących.

## Zobacz również

- Dokumentacja Javy dla klasy Random: https://docs.oracle.com/javase/10/docs/api/java/util/Random.html
- Przykładowy projekt z wykorzystaniem generowania liczb losowych w grze: https://www.javacodegeeks.com/2015/07/java-basic-game-loop-example.html