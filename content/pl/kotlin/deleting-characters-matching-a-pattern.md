---
title:                "Kotlin: Usuwanie znaków pasujących do wzoru"
simple_title:         "Usuwanie znaków pasujących do wzoru"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/kotlin/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Dlaczego

Często podczas pisania kodu spotykamy się z sytuacją, w której chcemy usunąć część tekstu odpowiadającą określonemu wzorcowi. W Kotlinie istnieje możliwość skorzystania z różnych funkcji, które ułatwią nam to zadanie. W tym artykule dowiesz się, dlaczego warto używać tych funkcji i jak je zastosować.

## Jak to zrobić

Do usunięcia znaków pasujących do wzorca można wykorzystać metodę `remove` lub `replace` na obiekcie typu String. Sprawdźmy przykładowy kod:

```Kotlin
val originalString = "Kotlin jest super!"
val newString = originalString.remove("super")
println(newString) // wypisze "Kotlin jest!"
```

W przypadku gdy chcemy usunąć więcej niż jeden wzorzec, możemy użyć funkcji `replace` wraz z wyrażeniem regularnym. Zobaczmy przykład:

```Kotlin
val originalString = "123-456-789"
val newString = originalString.replace(Regex("[0-9]"), "")
println(newString) // wypisze "---"
```

Na powyższych przykładach widać, że usuwanie znaków na podstawie wzorca jest bardzo proste i intuicyjne w użyciu.

## Deep Dive

W Kotlinie istnieje także możliwość wykorzystania funkcji `dropWhile` oraz `slice` do usuwania znaków na podstawie predykatu. Funkcja `dropWhile` pozwala na usunięcie wszystkich znaków do momentu spełnienia warunku, natomiast funkcja `slice` usuwa znaki na wybranej pozycji.

### dropWhile

```Kotlin
val originalString = "Kot jest najlepszym przyjacielem człowieka"
val newString = originalString.dropWhile { it != 'n' }
println(newString) // wypisze "najlepszym przyjacielem człowieka"
```

### slice

```Kotlin
val originalString = "Witaj, świecie!"
val newString = originalString.slice(4..10)
println(newString) // wypisze "świecie"
```

Warto również zwrócić uwagę na wydajność używanego rozwiązania, ponieważ często nie jest to najważniejszy aspekt przy pisaniu kodu. W przypadku dużych ilości danych, lepszym wyborem może być użycie wyrażeń regularnych w funkcji `replace`.

## Zobacz także

- "Kotlin dla początkujących" (https://developer.android.com/kotlin/resources)
- "Wprowadzenie do wyrażeń regularnych w Kotlinie" (https://kotlinlang.org/docs/reference/regular-expressions.html)