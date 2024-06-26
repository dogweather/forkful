---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:04:50.436775-07:00
description: "Jak to zrobi\u0107: Elixir oferuje prosty spos\xF3b na zwi\u0119kszenie\
  \ wielko\u015Bci litery ci\u0105g\xF3w za pomoc\u0105 swoich wbudowanych funkcji,\
  \ bez potrzeby u\u017Cywania\u2026"
lastmod: '2024-03-13T22:44:35.023314-06:00'
model: gpt-4-0125-preview
summary: "Elixir oferuje prosty spos\xF3b na zwi\u0119kszenie wielko\u015Bci litery\
  \ ci\u0105g\xF3w za pomoc\u0105 swoich wbudowanych funkcji, bez potrzeby u\u017C\
  ywania zewn\u0119trznych bibliotek."
title: "Zamiana liter na wielkie w \u0142a\u0144cuchu znak\xF3w"
weight: 2
---

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
