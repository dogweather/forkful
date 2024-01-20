---
title:                "Konwersja ciągu znaków na małe litery"
html_title:           "Fish Shell: Konwersja ciągu znaków na małe litery"
simple_title:         "Konwersja ciągu znaków na małe litery"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/elixir/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Co i dlaczego?

Konwersja tekstu na małe litery to proces, który zamienia każdą dużą literę w ciągu na jej odpowiednik z małej litery. Programiści używają tego, aby porównywać ciągi w sposób niezależny od wielkości liter, co jest przydatne przy np. sortowaniu ciągów lub porównywaniu haseł.

## Jak to zrobić:

Elixir ma wbudowaną funkcję `String.downcase/1` do konwersji tekstu na małe litery. Oto prosty przykład:

```elixir
tekst = "Przykład Tekstu"
IO.puts String.downcase(tekst)
```

Wynikiem powyższego kodu będzie:

```elixir
"przykład tekstu"
```

## Głębsze zrozumienie

Konwersja tekstu na małe litery nie jest nowym pojęciem w programowaniu, jest dostępna we wszystkich popularnych językach programowania w pewnej formie. W Elixirze używamy `String.downcase/1`, ale w innych językach programowania może to wyglądać nieco inaczej. Na przykład, w JavaScript konwersję na małe litery wykonuje się za pomocą metody `toLowerCase()`.

Jeśli chcesz przekształcić ciąg na małe litery, zwracając uwagę na specyficzne dla języka zasady, jak np. traktowanie polskich liter, możesz to zrobić za pomocą `String.downcase/2`. Przyjmuję dodatkowy argument, którym jest język do użycia przy konwersji. Dowiedz się więcej o `String.downcase/2` w [dokumentacji Elixir](https://hexdocs.pm/elixir/String.html#downcase/2).

## Zobacz także

- [Dokumentacja Elixir: String.downcase/1](https://hexdocs.pm/elixir/String.html#downcase/1)

- [Jak Elixir porównuje ciągi](http://learning-elixir.joekain.com/comparing-elixir-strings/)

- [Przetwarzanie tekstu w Elixir](https://www.tutorialspoint.com/elixir/elixir_strings.htm)

Pamiętaj, że dobrą praktykę programistyczną jest zapoznanie się z dokumentacją i nauka o możliwościach dostępnych funkcji.