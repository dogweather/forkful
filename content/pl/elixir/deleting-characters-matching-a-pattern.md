---
title:                "Usuwanie znaków pasujących do wzorca."
html_title:           "Elixir: Usuwanie znaków pasujących do wzorca."
simple_title:         "Usuwanie znaków pasujących do wzorca."
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/elixir/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Co & Dlaczego?
Usuwanie znaków pasujących do wzorca jest powszechnym zadaniem programistów. Polega ono na wyszukiwaniu i usuwaniu wszystkich wystąpień określonego wzorca w tekście lub ciągu znaków. Jest to przydatne, gdy chcemy oczyszczać dane lub zmieniać format tekstu.

## Jak to zrobić:
```Elixir
str = "Ala ma kota."
str |> String.replace(~r/a/, "") 
```
W tym przykładzie usuwamy wszystkie wystąpienia litery "a" z wyrażenia "Ala ma kota." i otrzymujemy wynik "l m kott.".

## Pełna analiza:
Usuwanie znaków pasujących do wzorca jest znane również pod nazwą "usuwanie na podstawie wzorca" (pattern-based removal). Jest to powszechne w wielu językach programowania, takich jak Elixir czy Ruby. Alternatywne metody obejmują wyszukiwanie i zamianę, które wymaga więcej kodu, oraz użycie machery RegEx (Regular Expression) dla bardziej złożonych zadań. W Elixir, możemy użyć operatora "~r" do utworzenia wyrażenia regularnego i funkcji "String.replace" do usuwania pasujących znaków.

## Zobacz też:
- Dokumentacja Elixir dla funkcji String.replace: https://hexdocs.pm/elixir/String.html#replace/3
- Poradnik o jak działa "usuwanie na podstawie wzorca" w Elixir: https://dev.to/codebeam/pllist-practical-guide-to-elixir-pattern-matching-based-list-removal-1hm5