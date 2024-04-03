---
date: 2024-01-26 00:54:55.941128-07:00
description: "Obs\u0142uga b\u0142\u0119d\xF3w to spos\xF3b, w jaki Tw\xF3j kod radzi\
  \ sobie z problemami, kt\xF3re pojawiaj\u0105 si\u0119 podczas wykonywania - jak\
  \ z\u0142apanie krzywej pi\u0142ki bez upuszczenia\u2026"
lastmod: '2024-03-13T22:44:35.372450-06:00'
model: gpt-4-1106-preview
summary: "Obs\u0142uga b\u0142\u0119d\xF3w to spos\xF3b, w jaki Tw\xF3j kod radzi\
  \ sobie z problemami, kt\xF3re pojawiaj\u0105 si\u0119 podczas wykonywania - jak\
  \ z\u0142apanie krzywej pi\u0142ki bez upuszczenia jej."
title: "Obs\u0142uga b\u0142\u0119d\xF3w"
weight: 16
---

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
