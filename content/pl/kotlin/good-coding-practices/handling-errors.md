---
title:                "Obsługa błędów"
aliases:
- pl/kotlin/handling-errors.md
date:                  2024-01-26T00:54:55.941128-07:00
model:                 gpt-4-1106-preview
simple_title:         "Obsługa błędów"

tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/kotlin/handling-errors.md"
---

{{< edit_this_page >}}

## Co i dlaczego?
Obsługa błędów to sposób, w jaki Twój kod radzi sobie z problemami, które pojawiają się podczas wykonywania - jak złapanie krzywej piłki bez upuszczenia jej. Programiści robią to, aby zapobiec awariom i zapewnić użytkownikom płynne doświadczenie.

## Jak to zrobić:
Kotlin zapewnia `try`, `catch`, `finally` oraz `throw` do zarządzania błędami. Oto jak ich użyć:

```Kotlin
fun main() {
    val licznik = 10
    val mianownik = 0

    try {
        val wynik = licznik / mianownik
        println("Wynik: $wynik")
    } catch (e: ArithmeticException) {
        println("Nie można dzielić przez zero, kolego.")
    } finally {
        println("To się stanie niezależnie od wszystkiego.")
    }
}
```

Wynik:
```
Nie można dzielić przez zero, kolego.
To się stanie niezależnie od wszystkiego.
```

Jeśli coś pójdzie nie tak w bloku `try`, wykonanie przeskakuje do `catch`. Łapie on konkretny błąd, który został rzucony (`ArithmeticException` w tym przypadku). Blok `finally` jest wykonywany po nich - bez względu na wynik.

## Wnikliwie
Blok `try-catch` jest używany od dawnych dni programowania – to jak sieć bezpieczeństwa. Kotlin oferuje również `throw` do ręcznego rzucania wyjątkiem, a także `finally` dla kodu, który musi zostać wykonany - często jest to praca porządkowa.

Alternatywy obejmują typ `Result` oraz `try` Kotlin jako wyrażenie.

```Kotlin
val wynik: Result<Int> = try {
    Result.success(licznik / mianownik)
} catch (e: ArithmeticException) {
    Result.failure(e)
}
```
To podejście zwraca obiekt `Result` – otrzymujesz albo sukces, albo porażkę, bez dramatu nieobsłużonego wyjątku.

Implementacja w Kotlin jest schludna, ponieważ można używać `try` jako wyrażenia, co oznacza, że zwraca wartość. Takie wybory sprawiają, że obsługa błędów w Kotlinie jest dość wszechstronna. Chodzi o wybór odpowiedniego narzędzia do pracy, tak jak w warsztacie.

## Zobacz również
- Dokumentacja Kotlin dotycząca Wyjątków: [Obsługa wyjątków Kotlin](https://kotlinlang.org/docs/exception-handling.html)
- Dokumentacja typu `Result` Kotlin: [Result Kotlin](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin/-result/)
-Efektywne Java, 3. wydanie, autorstwa Joshuy Bloch – świetne spojrzenie na wyjątki, aczkolwiek specyficzne dla Javy.
