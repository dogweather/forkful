---
title:                "Kotlin: Wycinanie podciągów"
simple_title:         "Wycinanie podciągów"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/kotlin/extracting-substrings.md"
---

{{< edit_this_page >}}

## Dlaczego?

Istnieje wiele sytuacji, w których konieczne jest pobranie części tekstu z większego ciągu znaków. Może to być np. wyświetlenie określonego fragmentu tekstu na stronie internetowej, bądź przetworzenie danych w aplikacji. W tym artykule dowiesz się, jak w łatwy sposób wyciągać podciągi dzięki językowi Kotlin.

## Jak to zrobić?

Aby wyodrębnić podciąg z tekstu w języku Kotlin, należy wykorzystać funkcję `substring()` oraz podać indeksy początku i końca interesującej nas części. Przykładowe wywołanie tej funkcji wyglądałoby następująco:

```Kotlin
val tekst = "Witaj w świecie programowania w Kotlin"
val podciag = tekst.substring(19, 26)

println(podciag) 
```

W powyższym kodzie przypisujemy do zmiennej `tekst` dłuższy ciąg znaków, a następnie wywołujemy funkcję `substring()` z dwoma argumentami - indeksem początkowym (19) oraz końcowym (26). W efekcie otrzymujemy podciąg "programo" i wyświetlamy go na ekranie.

W przypadku, gdy interesuje nas tylko część tekstu od danego indeksu do końca ciągu, możemy pominąć drugi argument funkcji `substring()`, a w jego miejscu użyć metody `length()`, która zwraca długość tekstu:

```Kotlin
val fragment = tekst.substring(7)
```

W powyższym przykładzie wyodrębniamy podciąg "świecie programowania w Kotlin" z zmiennej `tekst`.

## Deep Dive

Podczas wybierania podciągu z tekstu należy pamiętać o istnieniu tzw. indeksów z ujemnymi wartościami. Dzięki nim możemy wybierać część tekstu od końca, a nie od początku. Przykładowo, indeks `-1` odpowiada ostatniemu znakowi w tekście, `-2` przedostatniemu, itd.

```Kotlin
val tekst = "Witaj w świecie programowania w Kotlin"
val podciag = tekst.substring(19, -6)

println(podciag)
```

W powyższym przykładzie wyodrębniamy podciąg od 19. indeksu do przedostatniego znaku, czyli "programowaniu w".

## Zobacz także

Jeśli chcesz dowiedzieć się więcej o funkcji `substring()` w języku Kotlin, polecamy zapoznać się z oficjalną dokumentacją:  
- [Dokumentacja języka Kotlin](https://kotlinlang.org/docs/reference/basic-types.html#strings)
- [Przykładowy kod w języku Kotlin](https://github.com/JetBrains/kotlin-examples/blob/master/examples/strings/src/Example.kt)
- [Artykuł na Medium o wydajności funkcji `substring()`](https://medium.com/@MarioAriasC/qu%C3%A9-tan-caras-son-los-string-en-java-97da1dee3030)