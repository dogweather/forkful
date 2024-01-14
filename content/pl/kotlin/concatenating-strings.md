---
title:    "Kotlin: Łączenie ciągów znaków"
keywords: ["Kotlin"]
---

{{< edit_this_page >}}

## Dlaczego

Stringi są jednym z podstawowych typów danych w języku Kotlin. Często zdarza się, że musimy łączyć ze sobą dwa lub więcej stringów w celu utworzenia jednego większego. W tym artykule dowiesz się, dlaczego konkatynacja stringów jest ważna i jak możesz to zrobić w języku Kotlin.

## Jak to zrobić

W języku Kotlin istnieją różne metody łączenia stringów. Jedną z najprostszych jest użycie operatora "+" lub funkcji "plus()". Przykładowo:

```Kotlin
val imie = "Anna"
val nazwisko = "Nowak"
val pelneImie = imie + " " + nazwisko // używając operatora "+"
val inneImie = imie.plus(" ").plus(nazwisko) // używając funkcji "plus()"
```

W obu przypadkach wynikiem jest string "Anna Nowak". Możemy również wykorzystać metody formatowania stringów, takie jak "format()" lub "replace()". Przykładowo:

```Kotlin
val rok = 2021
val miesiac = "listopad"
val data = "Dzisiaj jest %s roku, a mamy już %s." // %s służy do zastępowania wartości
val sformatowanaData = data.format(miesiac, rok) // używając metody "format()"
val nowaData = data.replace("%s", miesiac).replace("%s", rok.toString()) // używając metody "replace()"
```

W obu przypadkach wynikiem jest string "Dzisiaj jest listopad roku, a mamy już 2021." Warto również pamiętać o użyciu funkcji "toString()" w przypadku łączenia stringów z innymi typami danych, takimi jak liczby czy znaki.

## Vertkanda szczegóły

Gdy połączysz stringi za pomocą operatora "+" lub metody "plus()", tworzony jest zupełnie nowy obiekt String. W przypadku formatowania stringów za pomocą funkcji "format()" lub "replace()", używany jest obiekt typu "StringBuilder", który jest bardziej wydajny, ponieważ nie tworzy nowych obiektów przy każdej konkatenacji.

Dodatkowo, w języku Kotlin istnieją również złożone metody łączenia stringów, takie jak "joinToString()", która pozwala na łączenie elementów z listy z użyciem wybranego separatora, lub "buildString()", która pozwala na tworzenie stringów bez konieczności używania metody "append()".

## Zobacz także

- [Dokumentacja języka Kotlin](https://kotlinlang.org/docs/home.html)
- [Oficjalny poradnik języka Kotlin](https://kotlinlang.org/docs/kotlin-docs.pdf)
- [Kotlin w akcji](https://www.manning.com/books/kotlin-in-action)

Dziękujemy za przeczytanie tego artykułu! Mam nadzieję, że dowiedziałeś/aś się czegoś nowego o łączeniu stringów w języku Kotlin. Jesteśmy również ciekawi, jak ty wykorzystujesz tę funkcjonalność w swoich projektach. Daj nam znać w komentarzu poniżej!