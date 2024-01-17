---
title:                "Generowanie losowych liczb"
html_title:           "Elixir: Generowanie losowych liczb"
simple_title:         "Generowanie losowych liczb"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/elixir/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Co & Dlaczego?
Generowanie losowych liczb jest ważną częścią programowania, ponieważ pozwala na tworzenie losowych i nieprzewidywalnych danych. Programiści często wykorzystują generowanie liczb losowych do testowania swoich kodów, symulacji różnych scenariuszy oraz do tworzenia gier czy losowań.

## Jak to zrobić:
```Elixir
rand = :rand.uniform(1..10)
IO.puts("Wylosowana liczba to #{rand}")
```

Sample output:
```
Wylosowana liczba to 4
```

## Głębszy zanurzenie:
Pierwszy algorytm do generowania liczb losowych został opracowany w 1946 roku przez Rudy'ego Goldberga i Gerarda Saltona. Alternatywne sposoby tworzenia liczb losowych to m.in. szum gaussowski i symulowane obliczenia kwantowe. W Elixir, generator pseudolosowy został zaimplementowany w module `:rand`, który oferuje różne funkcje do generowania losowych liczb.

## Zobacz też:
- Dokumentacja Elixir dla generatora pseudolosowego: https://elixir-lang.org/getting-started/random-numbers.html
- Biblioteka do generowania liczb losowych: https://hexdocs.pm/secure_random/SecureRandom.html
- Dyskusja na temat bezpieczeństwa generowania liczb losowych w Elixir: https://elixirforum.com/t/random-number-generation-in-elixir/13404/3