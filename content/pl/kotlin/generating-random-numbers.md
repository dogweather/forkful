---
title:                "Kotlin: Generowanie losowych liczb"
programming_language: "Kotlin"
category:             "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/kotlin/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Dlaczego

Generowanie losowych liczb może być przydatne w wielu różnych scenariuszach programistycznych. Może to być przydatne do testowania funkcji, symulacji, generowania unikalnych identyfikatorów lub losowego wybierania elementów z listy. W tym artykule dowiesz się, jak w łatwy sposób wykorzystać generowanie losowych liczb w języku Kotlin.

## Jak to zrobić

Aby wygenerować losową liczbę w zakresie od 0 do 10, możesz użyć poniższego kodu:

```Kotlin
val random = (0..10).random()
println(random)
```

Wynik powyższego przykładu może wyglądać następująco: 7. W tym przypadku, operator `..` służy do określenia zakresu, a metoda `random()` generuje losową liczbę w danym zakresie.

Jeśli chcesz wygenerować losową liczby np. z zakresu od 20 do 30, możesz użyć też metody `nextInt()` z klasy `Random`, tak jak w poniższym przykładzie:

```Kotlin
val random = Random().nextInt(11) + 20
println(random)
```

Powyższy przykład wygeneruje losową liczbę z zakresu od 20 do 30. Możesz dostosować zakres do swoich potrzeb, zmieniając argumenty w metodzie `nextInt()`.

Pamiętaj, że aby wykorzystać funkcje związane z generowaniem liczb losowych, musisz zaimportować klasę `Random` do swojego projektu.

```Kotlin
import java.util.Random
```

Możesz również użyć metody `nextBoolean()` aby wygenerować losową wartość logiczną (true/false), lub metody `nextDouble()` aby wygenerować losową liczbę zmiennoprzecinkową.

## Głębszy wgląd

Podczas generowania typowych liczb losowych, istnieje ryzyko wystąpienia tzw. "pętli pseudo losowości". Oznacza to, że pewne wartości mogą się powtórzyć w wyniku wielu wywołań metody `random()`. Aby temu zapobiec, można określić ziarno (seed) dla generatora liczb losowych. Ziarno jest liczbą, która służy jako punkt wyjściowy dla generowania liczb losowych i może być zdefiniowana jako argument w konstruktorze klasy `Random`.

Poniższy przykład pokazuje, jak można użyć ziarna do generowania losowego tekstu:

```Kotlin
val random = Random(12345)
val letters = "abcdefghijklmnopqrstuvwxyz"

repeat(10) {
    val index = random.nextInt(letters.length)
    val letter = letters[index]
    print(letter)
}
```

W powyższym przykładzie, wykorzystując ziarno `12345`, będzie zawsze generowany ten sam ciąg liter: `tqbgefpfip`.

## Zobacz również

- [Random Numbers in Kotlin](https://kotlinlang.org/docs/random-numbers.html)
- [Java Random class](https://docs.oracle.com/javase/8/docs/api/java/util/Random.html)