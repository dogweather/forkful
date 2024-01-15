---
title:                "Łączenie ciągów znaków"
html_title:           "Kotlin: Łączenie ciągów znaków"
simple_title:         "Łączenie ciągów znaków"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/kotlin/concatenating-strings.md"
---

{{< edit_this_page >}}

## Dlaczego

Jeśli programujesz w Kotlinie, prawdopodobnie często musisz manipulować ciągami znaków. Łączenie (czyli łączenie) dwóch lub więcej ciągów jest częstym wyzwaniem w wielu projektach. W tym artykule dowiesz się, jak łatwo i skutecznie połączyć ciągi znaków w Kotlinie.

## Jak to zrobić

Połączenie ciągów jest bardzo proste w Kotlinie. W celu połączenia dwóch ciągów, możesz użyć operatora `+` lub funkcji `plus()`.

```Kotlin
val hello = "Cześć"
val world = "świecie!"

println(hello + " " + world) // Output: Cześć świecie!
println(hello.plus(" ").plus(world)) // Output: Cześć świecie!
```

Jeśli potrzebujesz połączyć więcej niż dwa ciągi, możesz użyć metody `joinToString()`. Pozwala ona na wybranie separatora oraz ewentualnych prefiksu i sufiksu.

```Kotlin
val fruit = listOf("jabłko", "banan", "pomarańcza")
println(fruit.joinToString(separator = ", ", prefix = "Jemy: ", postfix = " dzisiaj!")) // Output: Jemy: jabłko, banan, pomarańcza dzisiaj!
```

Jeśli potrzebujesz bardziej zaawansowanego łączenia ciągów, Kotlin oferuje również funkcję `buildString()`. Pozwala ona na wygodne budowanie ciągu znaków przy użyciu bloku lambda.

```Kotlin
val numbers = mutableListOf("1", "2", "3")

val concatenated = buildString {
    append("Liczby: ")
    numbers.forEach { append(it) }
}

println(concatenated) // Output: Liczby: 123
```

## Deep Dive

Kotlin oferuje również szereg funkcji, które pozwalają na bardziej zaawansowane manipulacje ciągami, takie jak `replace()`, `substring()`, `split()` czy `contains()`. Funkcje te są często używane przy łączeniu ciągów i mogą ułatwić pracę z tekstem w Twoim kodzie.

Warto także pamiętać o tym, że w Kotlinie ciągi znaków są niemodyfikowalne, co oznacza, że przy każdej operacji na ciągu, tworzony jest nowy obiekt. Dbaj więc o wydajność swojego kodu i używaj funkcji, które nie wymagają tworzenia nowych obiektów, jeśli jest to możliwe.

## Zobacz też

- [Oficjalna dokumentacja Kotlina](https://kotlinlang.org/docs/reference/basic-types.html#strings)
- [Praca z ciągami znaków w Kotlinie - przykładowe zadania](https://www.programiz.com/kotlin-programming/strings) 
- [Kurs Kotlina - łączenie ciągów znaków](https://try.kotlinlang.org/#/Examples/Problems/Concatenate%20Strings/Concatenate%20Strings.kt)