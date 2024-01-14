---
title:    "Java: Porównywanie dwóch dat"
keywords: ["Java"]
---

{{< edit_this_page >}}

## Dlaczego

Porównywanie dwóch dat jest ważnym aspektem w programowaniu, szczególnie gdy pracujemy z danymi zawierającymi informacje o czasie. Może to pomóc nam w ustalaniu kolejności wydarzeń lub w filtrowaniu danych w naszej aplikacji. W tym artykule dowiesz się, jak porównywać daty w języku Java oraz jakie są podstawowe zasady porównywania dat.

## Jak To Zrobić

Aby porównać dwie daty w języku Java, musimy najpierw utworzyć obiekty typu `Date` za pomocą konstruktora lub metod statycznych. Następnie używamy metody `compareTo()` do porównania dwóch dat. Przyjrzyjmy się przykładowemu kodowi poniżej:

```
import java.util.Date;

public class PorownanieDat {

    public static void main(String[] args) {
        Date data1 = new Date(120, 10, 5); // 5 listopada 2020
        Date data2 = new Date(120, 10, 6); // 6 listopada 2020

        // Porównujemy daty za pomocą metody compareTo()
        int wynik = data1.compareTo(data2);

        if (wynik < 0) {
            System.out.println("Data 1 jest wcześniejsza od daty 2.");
        } else if (wynik > 0) {
            System.out.println("Data 2 jest wcześniejsza od daty 1.");
        } else {
            System.out.println("Daty są sobie równe.");
        }
    }
}
```

W powyższym przykładzie, korzystając z metody `compareTo()`, porównujemy daty `data1` (5 listopada 2020) z datą `data2` (6 listopada 2020). Następnie wyświetlamy odpowiedni komunikat w zależności od wyniku porównania.

**Output:**

```
Data 1 jest wcześniejsza od daty 2.
```

Inną metodą porównywania dat jest użycie metody `after()` lub `before()`, która zwraca wartość logiczną `true` lub `false`. Przykładowy kod wykorzystujący te metody wyglądałby następująco:

```
// Przykładowe daty
Date data1 = new Date(120, 1, 1); // 1 lutego 2020
Date data2 = new Date(120, 2, 1); // 1 marca 2020

// Sprawdzamy, czy data1 jest wcześniejsza od data2
boolean czyWczesniejsza = data1.before(data2);

if (czyWczesniejsza) {
    System.out.println("Data 1 jest wcześniejsza od daty 2.");
} else {
    System.out.println("Data 1 jest późniejsza lub równa dacie 2.");
}
```

**Output:**

```
Data 1 jest wcześniejsza od daty 2.
```

## Deep Dive

Podczas porównywania dat w języku Java musimy pamiętać o kilku ważnych rzeczach:

- Klasy `Date` oraz metody `compareTo()` i `equals()` są przestarzałe w języku Java 8. Zaleca się korzystanie z nowszej klasy `LocalDateTime` oraz metod `compareTo()` i `equals()` z pakietu `java.time`.
- Porównywanie dat w języku Java odbywa się na podstawie daty i godziny, a nie samego dnia. Dlatego przy porównywaniu daty nie bierzemy pod uwagę czasu, a jedynie porównujemy daty.
- Daty w języku Java są przechowywane jako liczby długości 8 bajtów (64 bity) reprezentujące liczbę milisekund, która upłynęła od 1 stycznia 1970 roku. Dzieje się tak ze względu na wydajność i prostotę porównywania dat.

## Zobacz