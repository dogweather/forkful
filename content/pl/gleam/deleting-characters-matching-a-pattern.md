---
title:    "Gleam: Usuwanie znaków odpowiadających wzorcowi"
keywords: ["Gleam"]
---

{{< edit_this_page >}}

## Dlaczego

Czasami w programowaniu zdarza się, że musimy usuwać znaki ze stringów, które pasują do określonego wzoru. Może to być potrzebne, na przykład, gdy chcemy wyciągnąć tylko cyfry lub litery z tekstu. W tym artykule dowiesz się, jak w języku programowania Gleam wykonać to zadanie.

## Jak to zrobić

Aby usunąć dopasowane znaki ze stringa w języku Gleam, możemy skorzystać z funkcji `String.replace`. Poniższy przykład przedstawia jak usunąć wszystkie liczby z podanego stringa:

```Gleam
let string = "Abc123Def"
let output = String.replace(string, "[0-9]", "")
```

W tym przykładzie użyliśmy wyrażenia regularnego `"[0-9]"`, które oznacza wszystkie cyfry od 0 do 9. Funkcja `String.replace` zastępuje wszystkie znalezione dopasowania pustym stringiem, dlatego w wyniku otrzymaliśmy `AbcDef`.

Możemy również użyć wyrażeń regularnych, aby wybrać konkretne znaki do usunięcia. Na przykład, jeśli chcemy usunąć średniki z tekstu, możemy użyć `String.replace(string, "[;]", "")`.

## Deep Dive

Więcej informacji na temat usuwania znaków ze stringów w języku Gleam możesz znaleźć w oficjalnej dokumentacji. Tam dowiesz się między innymi jak korzystać z wyrażeń regularnych oraz jak dostosowywać usuwanie znaków do swoich potrzeb.

## Zobacz również

- [Dokumentacja Gleam - String](https://lptk.github.io/gleam/stdlib/string.html)
- [Dokumentacja Gleam - Regex](https://lptk.github.io/gleam/stdlib/regex.html)
- [Inne sposoby usuwania znaków ze stringów w Gleam - przykłady](https://dev.to/kristinbaumann/removing-specific-characters-from-a-string-in-gleam-4d7j)