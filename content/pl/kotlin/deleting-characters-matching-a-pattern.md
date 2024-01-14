---
title:                "Kotlin: Usuwanie znaków pasujących do wzorca"
programming_language: "Kotlin"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/kotlin/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Dlaczego

Czasami w swoim kodzie programów w języku Kotlin możemy natknąć się na niechciane znaki. Mogą być one niepotrzebne lub wręcz zaszkodzić funkcjonowaniu naszej aplikacji. W takich sytuacjach przydatne jest usunięcie tych znaków. W tym artykule pokażę Ci, jak w łatwy sposób usunąć znaki, które pasują do określonego wzorca.

## Jak to zrobić

Wykorzystają do tego metody `replace()` oraz `replaceFirst()`. Najpierw jednak musimy utworzyć zmienną, która będzie przechowywała nasz ciąg znaków.

```Kotlin
val string = "Wykła@@d programowania@@y w języku Kotlin@@ są cenne!"
```

Naszym zadaniem jest usunięcie znaku `@` oraz wszystkich znaków, które następują po nim. W tym celu musimy ustalić odpowiedni wzorzec.

```Kotlin
val pattern = "@+.*"
```

W powyższym wzorcu użyliśmy `+`, aby określić, że usuwane mają być wszystkie znaki występujące po znaku `@`. Teraz możemy wykorzystać metodę `replace()` w celu usunięcia naszego wzorca.

```Kotlin
val result = string.replace(pattern, "")
```

Teraz w zmiennej `result` przechowujemy nasz nowy ciąg znaków, w którym usunięte zostają wszystkie znaki pasujące do wzorca. Sprawdźmy teraz, czy nasza metoda zadziałała poprawnie.

```Kotlin
println(result)
```

**Wynik:**

```
Wykłady programowania są cenne!
```

Możemy także wykorzystać metodę `replaceFirst()` w celu usunięcia tylko pierwszego dopasowania do wzorca.

```Kotlin
val result = string.replaceFirst(pattern, "")
```

## Głębszy wgląd

W powyższym przykładzie wykorzystaliśmy proste wyrażenia regularne, aby określić wzorzec dla naszych znaków do usunięcia. Jednak możliwości wyrażeń regularnych są dużo większe. Możemy wykorzystać różne symbole, aby określić dokładniej, które znaki mają zostać usunięte.

Jeśli chcesz dowiedzieć się więcej o wyrażeniach regularnych w języku Kotlin, polecam przeczytać ten artykuł: [Wyrażenia regularne w języku Kotlin](https://kotlinlang.org/docs/regexp.html).

## Zobacz także

- [Podstawy języka Kotlin](https://kotlinlang.org/docs/basic-syntax.html)
- [Metody stringów w języku Kotlin](https://kotlinlang.org/docs/reference/basic-types.html#string-templates)