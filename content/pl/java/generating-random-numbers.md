---
title:    "Java: Generowanie losowych liczb"
keywords: ["Java"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pl/java/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Dlaczego

Generowanie losowych liczb jest powszechnym zadaniem w programowaniu. Może być użyte do różnych celów, takich jak symulacje, generowanie losowych danych do testów i gier komputerowych. Pozwala na tworzenie zmiennych i wartości, które nie są stałe i mogą mieć dowolną wartość, co jest niezbędne w wielu zastosowaniach.

## Jak to zrobić

Generowanie losowych liczb w Javie jest proste dzięki użyciu klasy ```java.util.Random```. Najpierw musimy utworzyć nowy obiekt tej klasy, np. ```Random rand = new Random();```. Możemy następnie wykorzystać metody tej klasy do generowania liczb całkowitych, np. ```int randomNumber = rand.nextInt();```, lub liczb zmiennoprzecinkowych, np. ```double randomNumber = rand.nextDouble();```. Możemy również określić zakres generowanych liczb, używając odpowiednich metod, np. ```int randomNumberInRange = rand.nextInt(100);``` wygeneruje liczbę całkowitą z zakresu od 0 do 99. Poniżej znajdują się przykładowe kody i wyniki dla różnych metod generowania liczb losowych.

#### Przykładowy kod:

```Java
// Generowanie losowej liczby całkowitej
Random rand = new Random();
int randomNumber = rand.nextInt();

// Generowanie losowej liczby całkowitej z zakresu od 0 do 99
int randomNumberInRange = rand.nextInt(100);

// Generowanie losowej liczby zmiennoprzecinkowej
double randomDouble = rand.nextDouble();
```

#### Przykładowy wynik:

```
randomNumber = -2008323321
randomNumberInRange = 72
randomDouble = 0.345678
```

## Głębsze zagadnienia

Generowanie losowych liczb może być również wykorzystane do tworzenia losowych wartości logicznych. Możemy wykorzystać metodę ```nextBoolean()``` do wygenerowania wartości prawdziwej lub fałszywej. Ponadto, możemy również ustalić ziarno losowości, czyli początkową wartość dla generatora liczb losowych, używając metody ```setSeed()```. Ważne jest również zauważyć, że liczby generowane przez klasę ```Random``` są pseudo-losowe, ponieważ korzystają one ze specjalnego algorytmu do generowania liczb losowych.

## Zobacz również

- Dokumentacja klasy [```java.util.Random```](https://docs.oracle.com/javase/8/docs/api/java/util/Random.html)
- Poradnik na temat generowania losowych liczb w Javie: [Jak wygenerować losowe liczby w Javie](https://mkyong.com/java/java-generate-random-int-float-double-long-byte/)
- Przykładowe zastosowania generowania liczb losowych w programowaniu: [Wykorzystanie losowych liczb w programowaniu](https://stackify.com/generate-random-numbers-java/)