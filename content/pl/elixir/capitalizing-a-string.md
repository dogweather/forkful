---
title:                "Zamiana liter na wielkie w łańcuchu znaków"
aliases:
- pl/elixir/capitalizing-a-string.md
date:                  2024-02-03T19:04:50.436775-07:00
model:                 gpt-4-0125-preview
simple_title:         "Zamiana liter na wielkie w łańcuchu znaków"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/elixir/capitalizing-a-string.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Co i Dlaczego?

Zwiększanie wielkości pierwszej litery ciągu polega na zamianie pierwszej litery ciągu na wielką, przy jednoczesnym zapewnieniu, że reszta liter jest mała. Ta czynność jest powszechnie potrzebna do formatowania danych wejściowych użytkownika lub wyświetlania tekstu w interfejsach użytkownika, gdzie istotne są spójność i czytelność.

## Jak to zrobić:

Elixir oferuje prosty sposób na zwiększenie wielkości litery ciągów za pomocą swoich wbudowanych funkcji, bez potrzeby używania zewnętrznych bibliotek. Oto prosty przykład:

```elixir
string = "elixir programming"
capitalized_string = String.capitalize(string)
IO.puts capitalized_string
```

Wyjście:

```
Elixir programming
```

W przypadkach, gdy potrzebna jest większa kontrola lub bardziej skomplikowana logika kapitalizacji, możesz połączyć różne funkcje String. Na przykład, jeśli chcesz zwiększyć wielkość pierwszej litery każdego słowa w zdaniu, możesz podzielić zdanie na słowa, zwiększyć wielkość pierwszej litery każdego z nich, a następnie połączyć je z powrotem:

```elixir
sentence = "elixir is fun"
capitalized_sentence = sentence 
                        |> String.split() 
                        |> Enum.map(&String.capitalize/1) 
                        |> Enum.join(" ")

IO.puts capitalized_sentence
```

Wyjście:

```
Elixir Is Fun
```

Chociaż standardowa biblioteka Elixira pokrywa większość potrzeb, w przypadku bardziej subtelnego manipulowania tekstem, w tym zaawansowanej kapitalizacji ciągów, możesz zbadać zewnętrzne biblioteki, takie jak Cldr do internacjonalizacji, które mogą oferować zachowania kapitalizacji specyficzne dla lokalizacji.
