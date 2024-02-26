---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:04:50.436775-07:00
description: "Zwi\u0119kszanie wielko\u015Bci pierwszej litery ci\u0105gu polega na\
  \ zamianie pierwszej litery ci\u0105gu na wielk\u0105, przy jednoczesnym zapewnieniu,\
  \ \u017Ce reszta liter jest\u2026"
lastmod: '2024-02-25T18:49:33.446372-07:00'
model: gpt-4-0125-preview
summary: "Zwi\u0119kszanie wielko\u015Bci pierwszej litery ci\u0105gu polega na zamianie\
  \ pierwszej litery ci\u0105gu na wielk\u0105, przy jednoczesnym zapewnieniu, \u017C\
  e reszta liter jest\u2026"
title: "Zamiana liter na wielkie w \u0142a\u0144cuchu znak\xF3w"
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
