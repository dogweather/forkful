---
date: 2024-01-20 17:47:51.321877-07:00
description: "D\u0142ugo\u015B\u0107 ci\u0105gu znak\xF3w to liczba znak\xF3w w danym\
  \ tek\u015Bcie. Programi\u015Bci musz\u0105 to wiedzie\u0107, aby np. weryfikowa\u0107\
  \ czy dane wprowadzone przez u\u017Cytkownika pasuj\u0105 do\u2026"
lastmod: '2024-03-11T00:14:08.536803-06:00'
model: gpt-4-1106-preview
summary: "D\u0142ugo\u015B\u0107 ci\u0105gu znak\xF3w to liczba znak\xF3w w danym\
  \ tek\u015Bcie. Programi\u015Bci musz\u0105 to wiedzie\u0107, aby np. weryfikowa\u0107\
  \ czy dane wprowadzone przez u\u017Cytkownika pasuj\u0105 do\u2026"
title: "Znalezienie d\u0142ugo\u015Bci ci\u0105gu znak\xF3w"
---

{{< edit_this_page >}}

## Co i dlaczego?
Długość ciągu znaków to liczba znaków w danym tekście. Programiści muszą to wiedzieć, aby np. weryfikować czy dane wprowadzone przez użytkownika pasują do oczekiwanych formatów lub obsługiwać teksty zmiennej długości.

## Jak to zrobić:
W Kotlinie, uzyskanie długości stringa jest banalnie proste. Oto przykłady:

```kotlin
fun main() {
    val tekst = "Witaj, Świecie!"
    println("Długość ciągu: ${tekst.length}")
}

// Wyjście: Długość ciągu: 14
```

Potrzebujesz długości bez białych znaków? Oto jak:

```kotlin
fun main() {
    val tekst = " Koty to fajne zwierzęta "
    println("Długość ciągu bez białych znaków: ${tekst.trim().length}")
}

// Wyjście: Długość ciągu bez białych znaków: 21
```

## Głębsze spojrzenie
Długość ciągu znaków to podstawowa własność w wielu językach programowania, również w Kotlinie. Z historycznego punktu widzenia, to jedna z pierwszych funkcji, jaką musieli opanować programiści, kiedy zaczynali pracować z tekstem.

W Kotlinie, `.length` to właściwość klasy `String`, która zwraca wartość typu `Int`. Długość jest liczbą jednostek kodu UTF-16 reprezentujących ciąg. Alternatywą może być użycie metody `.count()`, która pozwala zliczać znaki na podstawie zadanego warunku:

```kotlin
val tekst = "Kotlin"
val liczbaLiter = tekst.count { it.isLetter() }
println(liczbaLiter) // 6
```

W kontekście międzynarodowym, gdzie mogą wystąpić ciągi zawierające znaki Unicode, warto zauważyć, że `.length` nie zawsze odpowiada liczbie wyświetlanych „znaków”, ponieważ niektóre znaki mogą być reprezentowane przez więcej niż jedną jednostkę kodu.

## Zobacz także
- Dokumentacja Kotlin na temat String: [String - Kotlin Programming Language](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin/-string/)
