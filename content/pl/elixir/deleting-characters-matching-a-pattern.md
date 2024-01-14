---
title:                "Elixir: Usuwanie znaków pasujących do wzorca"
simple_title:         "Usuwanie znaków pasujących do wzorca"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/elixir/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Dlaczego

Usuwanie znaków pasujących do wzorca jest powszechnie stosowaną techniką w wielu językach programowania, a także w Elixirze. Może to być przydatne, gdy chcemy rozdzielić ciąg znaków na mniejsze fragmenty, usunąć zbędne informacje lub po prostu zmienić format danych. 

## Jak to zrobić

W Elixirze istnieje wiele sposobów na usuwanie znaków pasujących do wzorca. Jednym z najprostszych sposobów jest użycie wbudowanej funkcji `String.replace/4`, która przyjmuje cztery argumenty: ciąg znaków, wzorzec do usunięcia, nowy wzorzec i opcje. Poniższy przykład pokazuje wykorzystanie tej funkcji, aby usunąć wszystkie cyfry z ciągu znaków:
```Elixir
string = "To jest przykład123 tekstu456, który zawiera liczby789"
String.replace(string, ~r/[0-9]/, "")
```
Jeśli chcemy usunąć tylko konkretny wzorzec, np. wszystkie znaki niebędące literami, możemy użyć funkcji `String.replace_leading/3` lub `String.replace_trailing/3`. Poniższy przykład pokazuje zastosowanie `String.replace_trailing/3` do usunięcia znaków `!` z końca ciągu znaków:
```Elixir
string = "To jest przykład tekstu!!!"
String.replace_trailing(string, "!", "")
```
Inną przydatną funkcją jest `String.replace_first/4`, która umożliwia usunięcie tylko pierwszego wystąpienia wzorca. 

## Deep Dive

Przed użyciem funkcji `String.replace/4`, należy pamiętać o podaniu odpowiednich opcji, takich jak `global: true`, jeśli chcemy usunąć wszystkie wystąpienia wzorca, lub `caseless: true`, jeśli wzorzec ma być niewrażliwy na wielkość liter. Możliwe jest również użycie wyrażeń regularnych w miejsce prostych wzorców, co daje większą kontrolę nad usuwanymi znakami. 

W przypadku bardziej zaawansowanych operacji na ciągach znaków, warto zapoznać się z biblioteką `Regex`, która umożliwia wykorzystanie pełnej mocy wyrażeń regularnych. 

## Zobacz również

- [Dokumentacja Elixir](https://hexdocs.pm/elixir/String.html#replace/4)
- [Kurs programowania w Elixirze](https://www.elixircasts.io/)
- [Przykład usunięcia danych z ciągu znaków w Elixirze](https://github.com/Ben-McLean/elixir-examples/blob/master/string_cleaning.exs)