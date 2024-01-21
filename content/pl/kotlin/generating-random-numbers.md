---
title:                "Generowanie liczb losowych"
date:                  2024-01-20T17:49:44.835664-07:00
model:                 gpt-4-1106-preview
simple_title:         "Generowanie liczb losowych"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/kotlin/generating-random-numbers.md"
---

{{< edit_this_page >}}

## What & Why? (Co i dlaczego?)
Generowanie liczb losowych to sposób na uzyskanie wartości, które są nieprzewidywalne. Programiści wykorzystują je dla różnorodnych celów: od testów, przez gry, aż po symulacje.

## How to: (Jak to zrobić:)
W Kotlinie generowanie liczb losowych jest proste. Oto przykłady:

```Kotlin
import kotlin.random.Random

fun main() {
    // Generowanie losowej liczby całkowitej
    val randomInt = Random.nextInt()
    println(randomInt)

    // Losowa liczba z zakresu 0 do 99
    val randomInRange = Random.nextInt(100)
    println(randomInRange)

    // Losowy boolean
    val randomBoolean = Random.nextBoolean()
    println(randomBoolean)

    // Losowy Double
    val randomDouble = Random.nextDouble()
    println(randomDouble)
}
```

Sample output (Przykładowe wyjście):
```
-1188957731
42
true
0.723589984668817
```

## Deep Dive (Wgłębiając się)
W Kotlinie losowość bazuje na klasie `Random` z pakietu kotlin.random, która została wprowadzona w wersji 1.3 jako udoskonalona alternatywa dla Javy `java.util.Random`. Korzystanie z `Random` jest bezpieczne w wątkach i daje możliwość tworzenia własnych ziaren (seed). Jeśli potrzebujesz zreplikować losowość, użyj ziarna:

```Kotlin
val seededRandom = Random(1234)
val number = seededRandom.nextInt()
println(number)
```

Losowość można uzyskać również z funkcji rozszerzeń (extension functions), np. dla list:

```Kotlin
val list = listOf(1, 2, 3)
val randomElement = list.random()
println(randomElement)
```

Pamiętaj, że każda metoda generowania liczb losowych jest pseudo-losowa, bazowana na algorytmie, i zawsze istnieje możliwość przewidzenia wyników, jeśli znasz ziarno i algorytm.

## See Also (Zobacz również)
- [Kotlin Random documentation](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.random/-random/index.html)
- [Blog Kotlin Academy o liczbach losowych](https://blog.kotlin-academy.com/)
- [Oracle's Java Tutorials - Class Random](https://docs.oracle.com/javase/tutorial/essential/concurrency/rando.html)