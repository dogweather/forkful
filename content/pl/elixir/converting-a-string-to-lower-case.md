---
title:    "Elixir: Konwersja ciągu znaków na małe litery"
keywords: ["Elixir"]
---

{{< edit_this_page >}}

## Dlaczego?
Konwersja ciągu znaków na małe litery jest ważną częścią programowania w Elixirze. Pozwala na ujednolicenie danych i ułatwienie porównywania tekstu, co jest niezbędne w wielu aplikacjach. W tym artykule dowiesz się, jak wykonać tę operację w Elixirze i jakie są jej zalety.

## Jak to zrobić?
Poniżej przedstawiamy kilka przykładów kodu, które pokazują, jak wykonać konwersję ciągu znaków na małe litery w Elixirze. Pamiętaj, że funkcja `String.downcase/1` jest wbudowana w język Elixir, więc nie musisz importować żadnych dodatkowych modułów.

```Elixir
# Przykład 1
iex> String.downcase("HELLO")
"hello"

# Przykład 2
iex> String.downcase("tHis Is a sAMple sTRing")
"tis is a sample string"

# Przykład 3
iex> String.downcase("ążćęłńóśź")   # łączy polskie znaki
"ążćęłńóśź"

# Przykład 4
iex> String.downcase("123ABC")
"123abc"
```

## Głębsza analiza
Funkcja `String.downcase/1` jest traktowana jako case-preserving, co oznacza, że zachowuje ona oryginalną wielkość liter w ciągu znaków. Dzięki temu możesz bez obaw używać jej w różnego rodzaju wyrażeniach regularnych czy innych operacjach, w których wielkość liter ma znaczenie.

Istnieje również opcjonalny argument `:accent` przyjmujący dwa możliwe wartości `:compatible` i `:normal` (domyślnie). W przypadku ustawienia go na `:compatible`, funkcja `String.downcase/1` przekształci również polskie znaki specjalne na ich "kompatybilne" odpowiedniki, co może być przydatne w niektórych przypadkach.

## Zobacz też
- Dokumentacja funkcji `String.downcase/1`: https://hexdocs.pm/elixir/String.html#downcase/1
- Wideo tutorial na temat pracy ze Stringami w Elixirze: https://www.youtube.com/watch?v=5VwPKsWDmXo
- Porównanie funkcji `String.downcase/1` z innymi metodami konwersji ciągu znaków: https://medium.com/@athityakumar/elixir-string-case-conversions-e323c7e7a800