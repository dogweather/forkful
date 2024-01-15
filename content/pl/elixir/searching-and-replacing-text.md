---
title:                "Wyszukiwanie i zamiana tekstu"
html_title:           "Elixir: Wyszukiwanie i zamiana tekstu"
simple_title:         "Wyszukiwanie i zamiana tekstu"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/elixir/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Dlaczego?

Dlaczego warto uczyć się wyszukiwania i zamiany tekstu w języku Elixir? Otóż, jest to niezbędna umiejętność, jeśli chcemy pracować z tekstowymi danymi w naszych programach. Wyszukiwanie i zamiana tekstu używane jest często przy przetwarzaniu plików, tworzeniu automatycznych raportów, czy w analizie danych.

## Jak to zrobić?

W języku Elixir, do wyszukiwania i zamiany tekstu używamy funkcji `String.replace/3`. Pierwszym argumentem jest nasz oryginalny tekst, drugim argumentem jest wyrażenie regularne, które chcemy znaleźć, a trzecim argumentem jest tekst, którym chcemy zamienić znalezione wyrażenie.

```Elixir 
original_text = "Cześć! Witaj w świecie Elixir!"
Regex.replace(~r/witaj/, original_text, "czesc")
# Wynik: "Cześć! Cześć w świecie Elixir!"
```

Możemy również wykorzystać tę samą funkcję do wielokrotnej zamiany w tekście, używając modułu `Regex`. Pierwszym argumentem jest wyrażenie regularne, a drugim jest lista par: [cecha, zastępowana wartość].

```Elixir
Regex.replace(~r/(@(\w+))/, "Mój nick to @joe", [capture: "____", replace: "____"])
# Wynik: "Mój nick to ____"
```

## Głębsze wgląd

Funkcja `String.replace/3` jest jedynie jednym z narzędzi, które oferuje język Elixir w celu manipulacji tekstem. Inne przydatne funkcje to między innymi `String.insert/4` do wstawiania tekstu w wyznaczone miejsce, `String.reverse/1` do odwrócenia tekstu lub `String.trim/1` do usunięcia białych znaków na początku i końcu tekstu.

Pamiętaj, że wyrażenia regularne mogą być bardzo potężnym narzędziem przy wyszukiwaniu i zamianie tekstu. W Elixir, używamy składni `~r/.../` aby utworzyć wyrażenie regularne. Możemy również skorzystać z funkcji `Regex.escape/1` aby uniknąć błędów związanych ze znakami specjalnymi w tekście.

## Zobacz również

- Dokumentacja języka Elixir na temat manipulacji tekstem: https://hexdocs.pm/elixir/String.html
- Przydatny poradnik dotyczący wyrażeń regularnych w Elixir: https://medium.com/elixir-magic/regex-in-elixir-f7c42d0c7c32