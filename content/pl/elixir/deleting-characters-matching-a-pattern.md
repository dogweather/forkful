---
title:                "Usuwanie znaków pasujących do wzorca"
html_title:           "C: Usuwanie znaków pasujących do wzorca"
simple_title:         "Usuwanie znaków pasujących do wzorca"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/elixir/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Co i dlaczego?

Usuwanie znaków pasujących do wzorca to proces odfiltrowywania znaków ze stringa, które pasują do określonego wzoru. Programiści robią to, aby przekształcić dane wejściowe na deseń, który jest łatwiejszy do przetwarzania.

## Jak to zrobić:

W Elixir mamy do dyspozycji wiele narzędzi, które mogą nam pomóc. Najbardziej podstawowym jest wyrażenie regularne (regex). Oto przykład:

```elixir
iex> String.replace("Ala ma kota", ~r/a/, "")
"Il m kot"
```

Tutaj używamy funkcji `String.replace/3`, do której przekazujemy string ("Ala ma kota"), wzorzec (w tym przypadku ~r/a/ - wszystkie litery 'a') oraz ciąg, którym chcemy zastąpić znalezione dopasowania (w tym przypadku pusty ciąg).

## Wgłąb tematu:

Niektóre z wykorzystywanych do usuwania znaków pasujących do wzorca technik mają swoje korzenie w językach takich jak Perl i Python. Dzięki temu, Elixir, jako młodszy język, ma możliwość czerpania z sprawdzonych rozwiązań.

Alternatywą dla wyrażeń regularnych są listy znaków. Są one jednak mniej elastyczne i skomplikowane w użyciu.

Zaletą wyrażeń regularnych w Elixir jest fakt, że są one kompilowane do bajtkodu BEAM (silnik wirtualnej maszyny Erlang), co znacznie przyspiesza ich działanie.

## Zobacz także:

1. Dokumentacja Elixir na temat wyrażeń regularnych: https://elixir-lang.org/getting-started/regex.html
2. Więcej o String.replace: https://hexdocs.pm/elixir/String.html#replace/3
3. Dokumentacja BEAM: https://erlang.org/doc/man/beam_disasm.html