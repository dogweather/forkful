---
title:    "Java: Generowanie losowych liczb"
keywords: ["Java"]
---

{{< edit_this_page >}}

# Dlaczego warto korzystać z generowania liczb losowych

Generowanie liczb losowych jest ważnym aspektem programowania, który może znacznie ułatwić tworzenie różnorodnych aplikacji. Daje możliwość stworzenia losowych danych, które są niezbędne w wielu różnych zastosowaniach, takich jak symulacje, gry losowe, testowanie kodu czy szyfrowanie danych.

## Jak to zrobić?

Aby wygenerować liczbę losową w Javie, musimy skorzystać z klasy `Random` z pakietu `java.util`. Poniższy przykład kodu pokazuje, jak wygenerować jedną liczbę losową z zakresu od 1 do 10:

```Java
import java.util.Random;

public class RandomNumberExample {

    public static void main(String[] args) {
        Random random = new Random();
        int randomNumber = random.nextInt(10) + 1;
        System.out.println("Wylosowana liczba: " + randomNumber);
    }
}
```

W powyższym przykładzie użyliśmy metody `nextInt()`, która zwraca losową liczbę całkowitą z przedziału od 0 do podanej liczby (w naszym przypadku 10). Dodając 1 do wyniku, uzyskujemy liczbę z zakresu od 1 do 10.

Możemy również wygenerować wiele liczb losowych jednocześnie, korzystając z pętli `for`:

```Java
for (int i = 0; i < 5; i++) {
    int randomNumber = random.nextInt(100);
    System.out.println("Wylosowana liczba: " + randomNumber);
}
```

Kod ten wygeneruje pięć liczb losowych z przedziału od 0 do 99.

Jeśli potrzebujemy liczb zmiennoprzecinkowych, możemy skorzystać z metody `nextDouble()`, która zwraca losową liczbę z przedziału od 0.0 (włącznie) do 1.0 (wyłącznie).

## Głębsze zagadnienia

Generowanie liczb losowych może wydawać się prostym zadaniem, ale w rzeczywistości jest to proces dość skomplikowany. Kluczowym elementem jest odpowiednie ustawienie ziarna (ang. seed), które jest podstawą dla generowania liczb losowych. Dzięki temu możemy uzyskać ciągłe, przewidywalne wyniki.

Dlatego ważne jest, aby uważnie dobierać ziarno i unikać jego powtarzania w różnych wywołaniach. W praktyce najlepiej jest użyć bieżącego czasu jako ziarna, na przykład:

```Java
Random random = new Random(System.currentTimeMillis());
```

Ważną kwestią jest również wybór odpowiedniego algorytmu do generowania liczb losowych. Dobrym wyborem jest algorytm Mersenne Twister, który jest szybki i zapewnia dobre rozkłady liczb.

## Zobacz również

- [Dokumentacja klasy Random w Javie](https://docs.oracle.com/javase/8/docs/api/java/util/Random.html)
- [Wpływ ziarna na generowanie liczb losowych](https://www.geeksforgeeks.org/pseudo-random-numbers-java-generate/)
- [Opis algorytmu Mersenne Twister](https://pl.wikipedia.org/wiki/MersenneTwister)