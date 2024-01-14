---
title:                "Kotlin: Znajdowanie długości ciągu znaków"
programming_language: "Kotlin"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/kotlin/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Dlaczego

Wielu programistów często musi operować na ciągach tekstu podczas pisania kodu. W takich sytuacjach, ważne jest, aby wiedzieć jak znaleźć długość danego ciągu, ponieważ jest to podstawowa operacja, która może być potrzebna w różnych scenariuszach. W tym artykule dowiesz się, jak w łatwy sposób znaleźć długość stringa w języku Kotlin.

## Jak to zrobić

W języku Kotlin, istnieje kilka sposobów na znalezienie długości ciągu tekstu. Jednym z nich jest użycie metody `length()` na obiekcie `String`. Przykładowy kod wykorzystujący tę metodę wygląda następująco:

```Kotlin
val string = "Hello World"
val length = string.length()

println(length) // Output: 11
```
W powyższym przykładzie, zmienna `string` przechowuje tekst, a następnie wywołujemy na niej metodę `length()`, aby znaleźć jej długość. Wynik jest przypisywany do zmiennej `length` i następnie wyświetlany w konsoli.

Innym sposobem jest użycie właściwości `length` na obiekcie `String`. Przykładowy kod wykorzystujący tę właściwość wygląda następująco:

```Kotlin
val string = "Hello World"
val length = string.length

println(length) // Output: 11
```

Ostatnią opcją jest użycie operatora `count()` na ciągu tekstu. Przykładowy kod wykorzystujący ten operator wygląda następująco:

```Kotlin
val string = "Hello World"
val length = string.count()

println(length) // Output: 11
```

Wszystkie powyższe metody i właściwości zwracają długość ciągu jako wynik, więc można wybrać tę, która wydaje się najbardziej odpowiednia w danym kontekście.

## Deep Dive

Podczas pracy z ciągami tekstowymi, często potrzebujemy nie tylko znaleźć ich długość, ale także przetwarzać je w różny sposób. W języku Kotlin, można wykorzystać wiele funkcji i metod do manipulacji stringami, takich jak `substring()`, `contains()`, czy `replace()`. Ważne jest, aby zawsze pamiętać o typie zwracanym przez daną funkcję lub metodę, ponieważ niektóre z nich mogą zwracać wartość `null`.

Ponadto, jeśli chcesz poznać jeszcze więcej możliwości manipulacji ciągami tekstowymi w języku Kotlin, warto przeczytać o wyrażeniach regularnych i jak można je wykorzystać do analizy i modyfikacji stringów.

## Zobacz także

- [Dokumentacja języka Kotlin - podstawowe operacje na stringach](https://kotlinlang.org/docs/reference/basic-types.html#arrays)
- [Porównanie wydajności różnych metod na znalezienie długości stringa w języku Kotlin](https://stackoverflow.com/questions/35796617/string-length-generation-kotlin-vs-java)
- [Przykłady wykorzystania wyrażeń regularnych w języku Kotlin](https://www.baeldung.com/kotlin-regular-expressions)