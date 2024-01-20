---
title:                "Korzystanie z wyrażeń regularnych"
html_title:           "Arduino: Korzystanie z wyrażeń regularnych"
simple_title:         "Korzystanie z wyrażeń regularnych"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/elixir/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Co i Dlaczego?

Regularne wyrażenia są to wzorce pomagające odnaleźć określone sekwencje znaków w tekście. Programiści wykorzystują je do walidacji danych, wyszukiwania, dzielenia tekstu i zastępowania fragmentów tekstu.

## Jak to zrobić:

Chcesz znaleźć wszystkie liczby w tekście? Spójrz na następujący kod:

```elixir
tekst = "Cena jabłek to 6 zł, gruszki kosztują 8 zł."
:re.run(tekst, ~r/\b\d+\b/, capture: :all_but_first) |> elem(1)
```

Wynik to:

```elixir
["6", "8"]
```

Wzorzec \b\d+\b oznacza dowolną liczbę całościową.

## Więcej Wiedzy:

Regularne wyrażenia (regex) powstały w latach 50. ubiegłego stulecia i wciąż są popularne, pomimo alternatyw, takich jak biblioteki do analizy tekstu. Regexy są wszechstronne, ale mogą być trudne do odczytania.

Elixir używa silnika regexów opartego o bibliotekę PCRE, która posiada takie funkcje jak lookahead i lookbehind.

Alternatywą dla regexów mogą być funkcje dostępne w Elixir, takie jak `String.split/1` czy `String.replace/3`.

## Zobacz także:

2. Tutorial o regularnych wyrażeniach: [RexEgg](http://www.rexegg.com/)
3. Weryfikacja poprawności regularnego wyrażenia: [RegExr](https://regexr.com/)