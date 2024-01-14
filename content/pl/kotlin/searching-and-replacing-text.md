---
title:                "Kotlin: Wyszukiwanie i zastępowanie tekstu"
programming_language: "Kotlin"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/kotlin/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Dlaczego

Każdy programista wie, że czasem konieczne jest przeszukiwanie i zastępowanie tekstu w plikach. Może to wynikać z konieczności dostosowania kodu do nowych wymagań lub po prostu z uwzględnienia błędów. W tym artykule dowiesz się, jak w prosty sposób wykonać te operacje dzięki językowi Kotlin.

## Jak to zrobić

W języku Kotlin istnieją dwa sposoby na przeszukiwanie i zastępowanie tekstu: z użyciem funkcji `replace` lub biblioteki Regex. W obu przypadkach użyjemy metody `replace`:

```
Kotlin
val text = "Hello World!"
val newText = text.replace("World", "Kotlin")
println(newText) // Output: Hello Kotlin!
```

Pierwszym argumentem metody `replace` jest tekst, którego szukamy, a drugi to tekst, którym chcemy go zastąpić. W powyższym przykładzie zmieniliśmy "World" na "Kotlin" i wypisaliśmy wynik na ekran. Proste, prawda?

Jednak jeśli chcesz przeszukiwać tekst z użyciem wyrażeń regularnych (regular expressions), musisz skorzystać z biblioteki Regex:

```
Kotlin
val text = "Ten artykuł jest napisany w języku Java."
val pattern = "Java".toRegex()
val newText = pattern.replace(text, "Kotlin")
println(newText) // Output: Ten artykuł jest napisany w języku Kotlin.
```

W tym przypadku szukamy słowa "Java" w tekście za pomocą metody `toRegex()` i zastępujemy je napisem "Kotlin". Wyjściowy tekst zawiera już zmienione słowo.

## Deep Dive

Biblioteka Regex w języku Kotlin oferuje wiele funkcji, które pozwalają na dokładne przeszukiwanie i manipulację tekstem z użyciem wyrażeń regularnych. Na przykład, możesz zastąpić wszystkie wystąpienia danego wyrażenia lub wyciągać z tekstu określone fragmenty z użyciem metody `replace` i grup wyrażeń (groups). Jeśli chcesz zgłębić temat, polecam zapoznać się z dokumentacją biblioteki Regex w oficjalnej dokumentacji Kotlin.

## Zobacz też

- [Dokumentacja biblioteki Regex w Kotlin](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/-regex/)
- [Wyrażenia regularne w języku Kotlin](https://kotlinlang.org/docs/reference/regular-sequences.html)