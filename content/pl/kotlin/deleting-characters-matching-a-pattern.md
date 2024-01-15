---
title:                "Usuwanie znaków odpowiadających wzorcowi"
html_title:           "Kotlin: Usuwanie znaków odpowiadających wzorcowi"
simple_title:         "Usuwanie znaków odpowiadających wzorcowi"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/kotlin/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Dlaczego
Czy kiedykolwiek zdarzyło Ci się chcieć usunąć wszystkie znaki w tekście, które pasują do określonego wzorca? Może chcesz pozbyć się zbędnych odstępów lub znaków specjalnych? Nie martw się, Kotlin ma wbudowaną funkcję, która pozwala na usunięcie znaków pasujących do określonego wyrażenia regularnego. To bardzo przydatne w różnych sytuacjach, od przetwarzania tekstu w aplikacji mobilnej po czyszczenie danych wejściowych w systemie.

## Jak to zrobić
Aby usunąć znaki pasujące do wzorca w Kotlin, wystarczy użyć funkcji `replace()` wraz z wyrażeniem regularnym i pustym ciągiem znaków, którym chcemy je zastąpić. Na przykład, jeśli chcemy pozbyć się wszystkich odstępów w tekście, możemy użyć następującego kodu:

```Kotlin
val text = "To jest przykładowy tekst z odstępami."
val noSpaces = text.replace("\\s".toRegex(), "")
println(noSpaces)
```

Wyjście z tego kodu to `Tojestprzykładtekstzodstępami.` Ponieważ wyrażenie regularne `"\s"` odpowiada wszystkim odstępom w tekście, a następnie zastępujemy je pustym ciągiem znaków, spowoduje to usunięcie wszystkich odstępów w tekście. Można również dostosować wyrażenie regularne do swoich potrzeb, np. zmieniając go na `"\\d"` dla usunięcia cyfr lub `"\\W"` dla usunięcia znaków specjalnych.

## Głębszy zanurzenie
Funkcja `replace()` wykonuje zastępowanie tylko w pierwszym pasującym fragmencie tekstu. Jeśli chcesz usunąć wszystkie pasujące znaki z tekstu, możesz użyć funkcji `replace()` ponownie lub funkcji `replaceAll()` zamiast niej. Funkcja `replaceAll()` jest dostępna tylko w wersji rozszerzonej dla kolekcji znaków i działa podobnie do `replace()`, ale zawsze zastępuje wszystkie dopasowania. Możesz również ulepszyć wyrażenie regularne, aby wymusić zastępowanie wszystkich dopasowań. Na przykład, jeśli chcesz usunąć wszystkie znaki poza literami w tekście, możesz użyć wyrażenia `"[^a-zA-Z]"`.

## Zobacz także
Więcej informacji na temat funkcji `replace()` i innych funkcji do przetwarzania tekstu w Kotlin znajdziesz w oficjalnej dokumentacji:
- [Dokumentacja funkcji replace() w Kotlin](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/replace.html)
- [Kotlin dla programistów języka Java - przetwarzanie tekstu](https://kotlinlang.org/docs/tutorials/kotlin-for-py/dsl.html#text-processing)