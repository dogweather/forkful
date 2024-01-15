---
title:                "Zmiana wielkości liter ciągu znaków"
html_title:           "Kotlin: Zmiana wielkości liter ciągu znaków"
simple_title:         "Zmiana wielkości liter ciągu znaków"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/kotlin/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Dlaczego

Czy kiedykolwiek zastanawiałeś się, dlaczego musisz używać dużych liter w swoich programach? Może chcesz wyświetlić imię użytkownika w nagłówku swojej aplikacji lub utworzyć listę produktów z nazwami zapisanymi z wielkich liter. Bez względu na powód, ten artykuł przybliży Ci jak w łatwy sposób zamienić małe litery na duże w języku Kotlin.

## Jak to zrobić

```Kotlin
fun capitalizeString(string: String): String {
    return string.toUpperCase()
}

fun main() {
    val name = "jan"
    println(capitalizeString(name))
}

// Output:
JAN
```

W powyższym przykładzie używamy funkcji `toUpperCase()` do zamiany wszystkich liter w ciągu znaków na wielkie. Następnie wywołujemy tę funkcję w funkcji `main()`, podając jej jako argument zmienną `name`. 

Możesz także użyć poniższego kodu, aby zamienić tylko pierwszą literę w ciągu znaków na dużą:

```Kotlin
fun capitalizeString(string: String): String {
    return string.capitalize()
}

fun main() {
    val name = "jan"
    println(capitalizeString(name))
}

// Output:
Jan
```

Funkcja `capitalize()` zamienia tylko pierwszą literę, więc możesz ją wykorzystać do stworzenia bardziej realistycznych imion w swoich aplikacjach.

## Wnikliwe studium

Teraz, gdy wiesz jak zamienić małe litery na duże w języku Kotlin, warto wiedzieć, że istnieje wiele innych metod, które mogą ułatwić pracę z ciągami znaków. Na przykład, jeśli chcesz zamienić tylko wybrane części ciągu na wielkie litery, możesz użyć metody `replace()`.

```Kotlin
fun capitalizeString(string: String): String {
    return string.replace("a", "A")
}

fun main() {
    val name = "jan"
    println(capitalizeString(name))
}

// Output:
JAn
```

Możesz także użyć metody `substring()`, aby wybrać konkretne litery w ciągu i zamienić je na duże.

```Kotlin
fun capitalizeString(string: String): String {
    val firstLetter = string.substring(0, 1)
    val restOfWord = string.substring(1)
    
    return firstLetter.uppercase() + restOfWord
}

fun main() {
    val name = "jan"
    println(capitalizeString(name))
}

// Output:
Jan
```

Powyższy przykład pokazuje, jak możesz wybrać pierwszą literę ciągu, zamienić ją na wielką i dodać ją do reszty słowa, które zostało pozostawione bez zmian. 

Warto także wspomnieć, że metoda `uppercase()` może być użyta jako skrót zamiast `toUpperCase()` i jest również dostępna w kodekach String Template w Kotlin, co pozwala na bardziej wygodne używanie jej w kodzie.

## Zobacz także

- Dokumentacja języka Kotlin: https://kotlinlang.org/docs/reference/
- Przewodnik po alfabetach i ciągach znaków w języku Kotlin: https://kotlinlang.org/docs/reference/basic-types.html#characters-and-strings
- Materiały edukacyjne i przykłady kodu w języku Kotlin: https://kotlinlang.org/docs/learn.html