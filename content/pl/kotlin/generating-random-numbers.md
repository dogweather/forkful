---
title:                "Generowanie losowych liczb"
html_title:           "Kotlin: Generowanie losowych liczb"
simple_title:         "Generowanie losowych liczb"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/kotlin/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Dlaczego

Generowanie liczb losowych jest nieodzownym elementem wielu zastosowań programistycznych, takich jak testowanie, sztuczna inteligencja, generowanie symulacji, gry itp. Jest to niezwykle przydatne narzędzie, które pozwala na tworzenie różnorodnych danych i scenariuszy, które mogą być wykorzystane w programowaniu.

## Jak to zrobić

```Kotlin
// Generator liczb losowych
val randomNumber = (1..10).random()
println(randomNumber)
// Wynik: 5
```

Aby wygenerować losową liczbę w języku Kotlin, możemy skorzystać z metody `random()` w połączeniu z zakresem liczb, z którego chcemy wybierać. W powyższym przykładzie wykorzystaliśmy zakres od 1 do 10, więc zostanie wygenerowana liczba z tego przedziału. Możemy również wykorzystać metodę `nextInt()` w połączeniu z maksymalną liczbą, którą chcemy wygenerować. Na przykład:

```Kotlin
// Generator liczb losowych
val randomNumber = Random.nextInt(20)
println(randomNumber)
// Wynik: 14
```

Możemy również generować losowe wartości zmiennoprzecinkowe korzystając z metody `nextDouble()`. Aby to zrobić, musimy określić przedział wartości, z którego chcemy wybierać. Przykład:

```Kotlin
// Generator wartości losowych zmiennoprzecinkowych
val randomValue = Random.nextDouble(1.0, 10.0)
println(randomValue)
// Wynik: 6.342
```

## Deep Dive

Generowanie liczb losowych jest możliwe dzięki wykorzystaniu tzw. generatorów pseudolosowych. Są to algorytmy, które na podstawie jednej liczby - tzw. ziarna (ang. seed) - generują ciąg liczb, które wydają się być losowe. W języku Kotlin generator losowych liczb jest zaimplementowany w klasie `Random`. Przy wywołaniu metody `random()` bez określenia ziarna, zostanie użyty domyślny ziarno - aktualny czas systemowy.

## Zobacz także

- [Dokumentacja metody random() w języku Kotlin](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.random/-random/index.html)
- [Poradnik użytkownika języka Kotlin - Generatory losowych liczb](https://kotlinlang.org/docs/reference/basic-types.html#random-number-generators)
- [Wprowadzenie do generowania liczb losowych w Java](https://www.baeldung.com/java-generating-random-numbers)