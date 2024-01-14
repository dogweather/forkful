---
title:                "Java: Tworzenie losowych liczb"
simple_title:         "Tworzenie losowych liczb"
programming_language: "Java"
category:             "Java"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/java/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Dlaczego

Generowanie losowych liczb jest ważną umiejętnością w programowaniu, ponieważ pozwala na tworzenie różnorodnych funkcji i aplikacji, które wymagają losowości. Jest to szczególnie użyteczne w grach, symulacjach i w wielu innych zastosowaniach. W tym artykule pokażemy jak w prosty sposób generować losowe liczby w języku Java.

## Jak to zrobić

Aby wygenerować losową liczbę w Java, musimy wykorzystać klasę "Random". Poniżej znajduje się kod, który generuje losową liczbę całkowitą z przedziału od 0 do 10 oraz wyświetla ją na ekranie:

```Java
import java.util.Random;

public class GenerowanieLosowychLiczb {
    public static void main(String[] args) {
        Random rand = new Random();
        int randomNumber = rand.nextInt(11);
        System.out.println("Wylosowana liczba to: " + randomNumber);
    }
}
```

Bardzo ważne jest użycie metody "nextInt()" po obiekcie klasy "Random", ponieważ ta metoda pozwala na wygenerowanie losowej liczby z określonego przedziału. W powyższym przykładzie, używając "11" jako argumentu metody "nextInt()", otrzymujemy liczbę z przedziału od 0 do 10.

Możemy również zastosować tę samą metodę z innymi typami danych, takimi jak "double" czy "float", aby uzyskać losowe liczby zmiennoprzecinkowe. Poniżej znajduje się przykładowy kod generujący losową liczbę zmiennoprzecinkową z przedziału od 0 do 1:

```Java
double randomDouble = rand.nextDouble();
```

Możemy również użyć metody "nextInt()" z argumentem "bound", aby ustalić górny limit dla wygenerowanej liczby. Na przykład, jeśli chcemy uzyskać losową liczbę z przedziału od 1 do 100, użyjemy takiego kodu:

```Java
int randomNumber = rand.nextInt(100) + 1;
```

To jest tylko podstawowe użycie klasy "Random" - istnieje jeszcze wiele innych metod, które można używać do różnych celów. Warto zwrócić uwagę na to, że klasy "Random" nie należy tworzyć wielokrotnie w programie, lecz raz dla całego programu, ponieważ inaczej otrzymane liczby nie będą już całkowicie losowe.

## Prześwietlenie

Technika generowania liczb losowych w języku Java wykorzystuje tzw. "pseudolosowy" generator, który wykorzystuje algorytmy do wytworzenia liczby wydającej się losowa. Istnieją różne algorytmy generowania liczb losowych, a kluczowym jest wybranie odpowiedniego dla naszego konkretnego przypadku. Należy również uważać, aby nie używać klasy "Random" do celów kryptograficznych, ponieważ algorytmy w niej zawarte nie są wystarczająco bezpieczne.

Warto również zwrócić uwagę, że w przypadku wykorzystania pojedynczej instancji klasy "Random" do wielu celów generowania liczb losowych, można uzyskać błędne wyniki. Dzieje się tak, ponieważ każde wywołanie metody "nextInt()" zmienia wewnętrzny stan generatora, co może mieć wpływ na kolejne generowane liczby. W takich przypadkach lepiej jest wykorzystać różne instancje klasy "Random" dla różnych celów.

## Zobacz też

- Dokumentacja kliasy "Random" w języku Java: https://docs.oracle.com/javase/8/docs/api/java/util/Random.html
- Przykłady wykorzystania generowania liczb losowych w języku