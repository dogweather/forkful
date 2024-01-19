---
title:                "Generowanie liczb losowych"
html_title:           "Gleam: Generowanie liczb losowych"
simple_title:         "Generowanie liczb losowych"
programming_language: "Java"
category:             "Java"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/java/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Co i dlaczego?

Generowanie liczb losowych oznacza tworzenie liczby, która nie jest przewidywalna lepiej niż przez losowy przypadek. Programiści robią to, aby wprowadzić element nieprzewidywalności w swoje aplikacje, symulować losowość rzeczywistości, czy też przeprowadzać testy losowe.

## Jak to zrobić:

Użyjemy klasy java.util.Random, aby wygenerować liczbę losową.

```Java
import java.util.Random;

public class Main {
  public static void main(String[] args) {
    Random rand = new Random();

    int rand_int1 = rand.nextInt(1000); 
    System.out.println("Wygenerowano Liczbę Losową: "+ rand_int1);
  }
}
```
Po uruchomieniu powyższego kodu, otrzymasz wyjście takie jak poniżej:

```Java
Wygenerowano Liczbę Losową: 745
```

Każdorazowe uruchomienie może zwrócić inny wynik.

## Pogłębione podejście:

**Historia:**
Losowość jest fundamentalnym elementem komputerowego myślenia od lat 40-tych XX wieku, pierwsze komputery miały już wbudowane generatory liczb losowych. W Javie, klasa Random jest dostępna od wersji Java 1.0.

**Alternatywy:**
Generator liczb losowych Java.util.Random nie jest jedynym sposobem na generowanie liczb losowych w Javie. Możemy również skorzystać z java.security.SecureRandom, który zapewnia większe bezpieczeństwo przez użycie mocniejszych algorytmów.

**Szczegóły Implementacji:**
Klasa java.util.Random generuje liczby losowe na podstawie określonego ziarna. Jeżeli nie ustawimy ziarna, używana jest wartość domyślna, która jest bieżącym czasem systemu.

## Zobacz też:

1. Przewodnik Oracle po klasie Random: [Kliknij tutaj](https://docs.oracle.com/javase/8/docs/api/java/util/Random.html)
2. Przewodnik Oracle po klasie SecureRandom: [Kliknij tutaj](https://docs.oracle.com/javase/8/docs/api/java/security/SecureRandom.html) 
3. Przewodnik Oracle po generowaniu losowych liczb całkowitych: [Kliknij tutaj](https://docs.oracle.com/javase/tutorial/essential/environment/numbers.html)