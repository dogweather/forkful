---
title:                "Generowanie liczb losowych"
html_title:           "Gleam: Generowanie liczb losowych"
simple_title:         "Generowanie liczb losowych"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/kotlin/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Co i dlaczego?

Generowanie losowych liczb to proces tworzenia liczb, które nie mają predictible patternu. Programiści robią to, aby wprowadzić losowość do swojego kodu, co jest niezbędne w wielu aplikacjach, takich jak gry, symulacje i bezpieczeństwo.

## Jak to zrobić:

Stosujemy tutaj wbudowane metody Koltina do generowania liczb losowych. Na przykład, aby wygenerować losową liczbę całkowitą od 1 do 10:

```Kotlin
val randomInteger = (1..10).random()
println(randomInteger)
```

Lub losowa liczba zmiennoprzecinkowa:

```Kotlin
val randomDouble = Math.random()
println(randomDouble)
```

## Deep Dive:

Historia generowania liczb losowych jest głęboko zakorzeniona w statystyce i matematyce. Pierwsze metody opierały się na rzeczywistych procesach losowych, takich jak rzuty kośćmi lub losowanie z kapelusza. Dziś, generowanie liczb losowych jest często osiągane za pomocą algorytmów komputerowych.

Alternatywą dla wbudowanych funkcji Koltina jest użycie zewnętrznej biblioteki, takiej jak "java.util.Random" lub "kotlinx.random". Te biblioteki mogą oferować większą kontrolę i rozszerzone funkcje.

Ważnym aspektem generowania liczb losowych jest ich "losowość”. W wielu przypadkach, liczby generowane komputerowo nie są prawdziwie losowe, ale są "pseudolosowe". To oznacza, że mają losowy wygląd, ale są generowane za pomocą określonego algorytmu. W praktyce jest to zazwyczaj wystarczające, ale w pewnych przypadkach (na przykład w kryptografii) może to być problemem.

## Zobacz także:

- Pakiet `kotlin.random` w dokumentacji Koltina: [link](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.random/)
- Generowanie liczb losowych w `java.util.Random`: [link](https://docs.oracle.com/javase/8/docs/api/java/util/Random.html)
- Dyskusja na temat losowości w generowaniu liczb losowych: [link](https://en.wikipedia.org/wiki/Random_number_generation#Pseudorandom_numbers)