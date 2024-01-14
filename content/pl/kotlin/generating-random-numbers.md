---
title:    "Kotlin: Generowanie losowych liczb"
keywords: ["Kotlin"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pl/kotlin/generating-random-numbers.md"
---

{{< edit_this_page >}}

# Dlaczego generowanie losowych liczb jest ważne

Generowanie losowych liczb jest nieodłączną częścią programowania w różnych językach, w tym także w Kotlinie. Może się wydawać, że generowanie losowych liczb jest tylko potrzebne w grach czy symulacjach, jednak jest to też kluczowa funkcjonalność w wielu innych zastosowaniach. W tym artykule dowiesz się, jak w prosty sposób generować losowe liczby w Kotlinie i jakie są jej główne zastosowania.

## Jak to zrobić

Aby wygenerować losowe liczby w Kotlinie, musimy skorzystać z wbudowanej funkcji `Random().nextInt()` oraz ustalić zakres, w którym chcemy wygenerować liczbę. Poniżej przedstawiam przykładowy kod, który wygeneruje liczbę całkowitą z przedziału od 0 do 10:

```Kotlin
val rand = Random().nextInt(11) // wygeneruj liczbę całkowitą od 0 do 10
println("Wylosowana liczba to $rand") // wyświetl wygenerowaną liczbę
```

Jeśli chcemy wygenerować liczbę z przedziału innych niż całkowite, możemy skorzystać z funkcji `Random().nextDouble()` lub `Random().nextFloat()`. Przykładowy kod generujący liczbę z zakresu od 0 do 1, z dokładnością do dwóch miejsc po przecinku, wyglądałby następująco:

```Kotlin
val rand = Random().nextDouble()  // wygeneruj liczbę z zakresu od 0 do 1
println("Wylosowana liczba to %.2f".format(rand)) // wyświetl wygenerowaną liczbę z dwoma miejscami po przecinku
```

## Pogłębione omówienie

W przypadku generowania losowych liczb, ważną kwestią jest ustawianie ziarna (seed) generatora, który ma wpływ na wygenerowane liczby. W Kotlinie możemy to zrobić za pomocą funkcji `Random(seed)`. Ustawienie tego samego ziarna spowoduje wygenerowanie tych samych liczb przy każdym uruchomieniu aplikacji. Jeśli chcesz uzyskać różne liczby przy każdym uruchomieniu, warto użyć urządzenia zewnętrznego, takiego jak czas komputera, jako ziarna.

Kolejną ważną kwestią jest wybór odpowiedniej metody generowania liczb losowych. W Kotlinie możemy skorzystać z trzech głównych metod: `Random.nextInt()`, `Random.nextDouble()` oraz `Random.nextBytes()`. Wybór odpowiedniej metody zależy od potrzeby danego projektu.

## Zobacz też

- Dokumentacja Kotlina na temat generowania losowych liczb: https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.random/-random/index.html
- Wideo "Kotlin Basics: Random Numbers": https://www.youtube.com/watch?v=s6SH72uAn3Q
- Przykładowy projekt wykorzystujący generowanie losowych liczb w Kotlinie: https://github.com/KotlinBy/Android-Kotlin-Examples/tree/master/RandomNumbers