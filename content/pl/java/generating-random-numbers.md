---
title:                "Java: Generowanie losowych liczb"
programming_language: "Java"
category:             "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/java/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Dlaczego

Generowanie losowych liczb jest powszechnie używane w programowaniu do symulowania rzeczywistych zdarzeń, testowania kodu i tworzenia gier. Może być również wykorzystane do losowego wybierania elementów ze zbioru lub do ukrywania danych w losowych miejscach.

## Jak to zrobić

Aby wygenerować losową liczbę w zakresie od 0 do 100 w języku Java, należy wykorzystać metodę `nextInt()` z klasy `Random`. Poniżej przedstawiony jest przykładowy kod:

```Java
import java.util.Random;

public class RandomNumbers {
    public static void main(String[] args) {
        Random rand = new Random();
        int randomNum = rand.nextInt(101);
        System.out.println(randomNum);
    }
}
```

Po uruchomieniu powyższego kodu powinniśmy otrzymać losową liczbę z przedziału od 0 do 100.

## Wnikliwa analiza

Podczas generowania losowych liczb, ważne jest aby korzystać z odpowiednich funkcji i algorytmów. W języku Java znajduje się klasa `Random`, która oferuje wiele metod do wygenerowania losowych liczb całkowitych, zmiennoprzecinkowych, jakościowych i wielu innych. Istnieją również alternatywne biblioteki takie jak Apache Commons, które oferują bardziej zaawansowane funkcje do generowania liczb.

Innym ważnym aspektem jest ustawianie ziarna (seed) dla generatora liczb losowych. Ziarno to jest początkową wartością używaną do wygenerowania sekwencji liczb losowych. Jeśli nie zostanie zadane, wówczas używane jest aktualne systemowe zegar komputera. Może to spowodować, że generowane liczby nie będą wystarczająco losowe. Dlatego zaleca się ustawienie własnego ziarna, na przykład na podstawie aktualnego czasu systemowego.

## Zobacz również

- [Dokumentacja Javy: Klasa Random](https://docs.oracle.com/javase/8/docs/api/java/util/Random.html)
- [Apache Commons - Generowanie losowych liczb](https://commons.apache.org/proper/commons-math/userguide/random.html)