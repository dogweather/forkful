---
title:                "Kotlin: Wyciąganie podciągów"
programming_language: "Kotlin"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/kotlin/extracting-substrings.md"
---

{{< edit_this_page >}}

## Dlaczego

Extraction of substrings, or taking specific parts of a larger string, is a common task in programming. It allows for more precise manipulation and analysis of data. In Kotlin, there are several built-in methods and functions to easily extract substrings. 

## Jak to zrobić

Aby wyodrębnić podciągi w Kotlinie, możemy skorzystać z dwóch głównych metod: `substring()` oraz `subSequence()`. Obie metody przyjmują dwa parametry: indeks początku i indeks końca, które określają fragment, który chcemy wyodrębnić.

```
Kotlin val sentence = "Programowanie w Kotlinie jest super!"

val result = sentence.substring(16, 23)
println(result) // wyświetli "Kotlinie"

val result2 = sentence.subSequence(23, 28)
println(result2) // wyświetli "jest "
```

Jeśli chcemy wyodrębnić całą resztę stringa, możemy pominąć drugi parametr. Wtedy metody `substring()` i `subSequence()` automatycznie wybiorą resztę stringa od podanego indeksu do końca. Możemy również wykorzystać metody `first()` i `last()` do szybkiego wyodrębnienia pierwszego i ostatniego znaku stringa.

```
Kotlin val sentence = "Hello World!"

val result = sentence.substring(6)
println(result) // wyświetli "World!"

val result2 = sentence.subSequence(sentence.first(), sentence.last())
println(result2) // wyświetli "Hello World!"
```

## Głębszy zewchód

Podczas wyodrębniania podciągów, warto pamiętać o tym, że metoda `substring()` tworzy nowy string, natomiast `subSequence()` zwraca nowy obiekt `CharSequence`. Dodatkowo, należy zwrócić uwagę na to, że indeks pierwszego znaku jest równy `0`, a ostatniego jest `length - 1`.

## Zobacz także

- Dokumentacja Kotlina na temat wyodrębniania podciągów: https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/index.html#substring
- Wideo "Kotlin Tips & Tricks - Wyodrębnianie podciągów": https://www.youtube.com/watch?v=Hy-VJbHN1T8
- Przykładowe zadania z wykorzystaniem wyodrębniania podciągów w Kotlinie: https://codeforces.com/problemset/tags/implementation?order=BY_SOLVED_DESC