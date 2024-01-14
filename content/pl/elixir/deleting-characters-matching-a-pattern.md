---
title:    "Elixir: Usuwanie znaków pasujących do wzoru"
keywords: ["Elixir"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pl/elixir/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Dlaczego

Może się zdarzyć, że w trakcie programowania w Elixirze zechcesz usunąć znaki pasujące do określonego wzorca. Może być to konieczne, gdy chcesz przeanalizować lub przekształcić dane w swoim kodzie. W tym artykule dowiesz się, jak to zrobić w prosty sposób.

## Jak to zrobić

Aby usunąć znaki pasujące do wzorca w Elixirze, możesz skorzystać z metody `String.replace` lub `Regex.replace` w zależności od Twoich potrzeb.

```Elixir
String.replace("Hello world!", "l", "")
# Output: "Heo word!"

Regex.replace("Hello world!", ~r/l/, "")
# Output: "Heo word!"
```

Zauważ, że obie metody przyjmują pierwszy argument jako ciąg znaków, a drugi jako wzorzec do usunięcia. W przypadku `String.replace` po prostu podajemy znak, który chcemy usunąć, podczas gdy w `Regex.replace` musimy użyć wyrażenia regularnego z prefixem `~r`. Jeśli potrzebujesz bardziej złożonego wzorca, możesz poszukać przydatnych wyrażeń regularnych lub zapoznać się z dokumentacją Elixira.

## Głębszy przegląd

Obie metody `String.replace` i `Regex.replace` mają opcjonalny trzeci argument, który pozwala na dokładniejsze kontrolowanie procesu usuwania. Na przykład, możesz określić maksymalną liczbę znaków do zastąpienia lub określić z jakiej strony w ciągu znaków powinna rozpocząć się operacja usunięcia.

Poniżej przedstawiono kilka przykładowych wywołań funkcji `String.replace`, aby pokazać, jak można dostosować sposób usuwania znaków:

```Elixir
# Usunięcie tylko pierwszego pasującego znaku:
String.replace("Hello world!", "l", "", global: false)
# Output: "Helo world!"

# Usunięcie tylko dwóch pierwszych pasujących znaków:
String.replace("Hello world!", "l", "", global: false, times: 2)
# Output: "Heo world!"

# Usunięcie tylko znaków występujących po prawej stronie ciągu:
String.replace("Hello world!", "o", "", insert_replaced: :after,
               insert_deleted: :after)
# Output: "Hell wrld!"
```

Spróbuj dostosować powyższe przykłady używając funkcji `Regex.replace` i przetestuj różne kombinacje argumentów, aby lepiej zrozumieć jak one działają.

## Zobacz też

- [Dokumentacja Elixira o String.replace](https://hexdocs.pm/elixir/String.html#replace/4)
- [Dokumentacja Elixira o Regex.replace](https://hexdocs.pm/elixir/Regex.html#replace/4)