---
title:                "Kotlin: Generowanie losowych liczb"
simple_title:         "Generowanie losowych liczb"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/kotlin/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Dlaczego

Generowanie liczb losowych jest nieodłączną częścią programowania, szczególnie w przypadku gier, symulacji, kryptografii i wielu innych zastosowań. Dzięki temu możemy uzyskać różnorodność i nieprzewidywalność w naszych aplikacjach, co jest niezbędne w wielu przypadkach.

## Jak to zrobić

Język programowania Kotlin posiada wiele wbudowanych funkcji do generowania liczb losowych. Jedną z nich jest funkcja ```random()```, która zwraca liczbę z przedziału od 0 do 1. Możemy również określić własny zakres poprzez wykorzystanie funkcji ```nextInt()``` lub ```nextDouble()```. Przykładowy kod wykorzystujący te funkcje wyglądałby następująco:

```Kotlin
fun main() {
    val randomNumber = (0..10).random()
    println("Wylosowana liczba: $randomNumber")
}
```

Wynik wykonania kodu będzie wyglądać mniej więcej tak:

```
Wylosowana liczba: 7
```

## Deep Dive

Generator liczb losowych wykorzystuje algorytm do wygenerowania pseudolosowych wartości. Oznacza to, że wyniki nie są całkowicie losowe, ale wykorzystują pewne wzorce, które są trudne do przewidzenia. W języku Kotlin wykorzystywany jest algorytm Mersenne Twister, który jest bardzo skuteczny w generowaniu liczb o znacznej losowości.

Kluczowym elementem wykorzystywanym w generowaniu liczb losowych jest tzw. "ziarno" (ang. seed). Jest to wartość, która jest wykorzystywana do inicjalizacji generatora i determinuje generowane liczby. Dzięki temu można uzyskać ten sam ciąg liczb, jeśli użyjemy tego samego ziarna. Możemy określić własne ziarno poprzez ustawienie parametru w funkcji ```Random()```. Przykładowo:

```Kotlin
fun main() {
    val randomSeed = Random(1234)
    for(i in 0..5) {
        println(randomSeed.nextInt(100))
    }
}
```

Wynik działania tego kodu będzie zawsze taki sam:

```
32
98
70
66
77
87
```

## Zobacz również

Jeśli chcesz dowiedzieć się więcej o generowaniu liczb losowych w języku Kotlin, polecam zapoznanie się z dokumentacją oficjalną: [https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.random/](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.random/)

Jeśli interesuje Cię zagadnienie generowania liczb losowych w innych językach programowania, możesz przeczytać ten artykuł: [https://www.baeldung.com/java-generating-random-numbers](https://www.baeldung.com/java-generating-random-numbers)