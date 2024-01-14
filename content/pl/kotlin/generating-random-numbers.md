---
title:    "Kotlin: Tworzenie losowych liczb"
keywords: ["Kotlin"]
---

{{< edit_this_page >}}

# Dlaczego?

Generowanie liczb losowych jest ważnym aspektem programowania, ponieważ pozwala na tworzenie różnorodnych i nieliniowych danych, co jest szczególnie przydatne w przypadku testowania algorytmów i aplikacji. Ponadto, generowanie liczb losowych jest również składnikiem kluczowym w grach i symulacjach.

# Jak to zrobić?

W języku Kotlin istnieje wiele sposobów na generowanie liczb losowych. Jednym z najprostszych sposobów jest użycie funkcji `random()` z pakietu `kotlin.math`. Przykładowy kod wykorzystujący tę funkcję wygląda następująco:
```Kotlin
import kotlin.math.random

fun main() {
    val randomNumber = random()
    println("Wygenerowana losowa liczba: $randomNumber")
}
```
Wywołanie funkcji `random()` zwróci liczbę zmiennoprzecinkową z zakresu od 0.0 do 1.0. Jeśli chcemy wygenerować liczbę całkowitą z konkretnego przedziału, możemy użyć funkcji `nextInt()` z pakietu `kotlin.random`. Przykładowy kod wykorzystujący tę funkcję wygląda następująco:
```Kotlin
import kotlin.random.Random

fun main() {
    val randomNumber = Random.nextInt(1, 100)
    println("Wygenerowana losowa liczba całkowita: $randomNumber")
}
```
W powyższym przykładzie wygenerujemy liczbę całkowitą z przedziału od 1 do 100.

# Deep Dive

Generowanie liczb losowych opiera się na wykorzystaniu generatorów liczb pseudolosowych. W języku Kotlin, funkcje `random()` i `nextInt()` korzystają z generatora liczb pseudolosowych typu Mersenne Twister, który jest uważany za jeden z najbardziej nieprzewidywalnych i szybkich generatorów liczb pseudolosowych.

Jeśli potrzebujemy większej kontroli nad sposobem generowania liczb losowych, możemy utworzyć własną klasę implementującą interfejs `Random`. Możemy wtedy zdefiniować własny algorytm generujący liczby pseudolosowe.

# Zobacz również

- Dokumentacja języka Kotlin na temat generowania liczb losowych: https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.random/
- Przykładowe wykorzystanie generowania liczb losowych w grze w języku Kotlin: https://github.com/libgdx/libgdx/wiki/Random-number-generator