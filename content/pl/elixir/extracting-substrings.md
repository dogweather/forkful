---
title:                "Wycinanie podciągów"
html_title:           "Elixir: Wycinanie podciągów"
simple_title:         "Wycinanie podciągów"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/elixir/extracting-substrings.md"
---

{{< edit_this_page >}}

## Dlaczego

Często w procesie programowania musimy operować na fragmentach tekstu - czy to dla wyciągania konkretnych informacji, czy też dla zmiany formatu danych. W takich przypadkach bardzo przydatne może okazać się wykorzystanie funkcji do wydobywania podłańcuchów z tekstu w języku Elixir.

## Jak to zrobić

W Elixir istnieje wiele sposobów na wydobywanie podłańcuchów z tekstu, ale najczęściej używanym jest funkcja `slice/3`, która pozwala nam na wybieranie fragmentów tekstu na podstawie indeksów. Przykładowo, jeśli chcemy wydobyć z tekstu nazwisko użytkownika, który znajduje się między znakami `<>`, możemy to zrobić w następujący sposób:

```Elixir
text = "<Jan Kowalski>"
surname = String.slice(text, 5, -2)
IO.puts(surname) # Kowalski
```

W powyższym przykładzie wykorzystujemy funkcję `slice/3`, która jako argumenty przyjmuje tekst, początkowy i końcowy indeks wybierania. W naszym przypadku używamy indeksów `5` oraz `-2`, co oznacza, że wybieramy fragment tekstu od 5. znaku do przedostatniego. Dzięki temu otrzymujemy oczekiwane nazwisko użytkownika.

Warto również wspomnieć o funkcji `match?/2`, która pozwala nam na sprawdzenie, czy podany tekst pasuje do danego wzorca. Przykładowo, jeśli chcemy sprawdzić, czy podany tekst jest adresem email, możemy to zrobić w ten sposób:

```Elixir
email = "jan.kowalski@example.com"
is_valid = String.match?(email, ~r/[\w.]+@[\w.]+\.[a-z]{2,3}/)
IO.puts(is_valid) # true
```

W powyższym przykładzie wykorzystujemy funkcję `match?/2`, która przyjmuje tekst oraz wzorzec do sprawdzenia. W naszym przypadku używamy wyrażenia regularnego, które pozwala nam na określenie formatu adresów email. Jeśli tekst będzie pasował do wzorca, funkcja zwróci wartość `true`, w przeciwnym razie będzie to `false`.

## Deep Dive

W Elixir istnieje wiele innych funkcji związanych z wydobywaniem podłańcuchów z tekstu, takich jak `substring/3`, `split/2`, czy `replace/3`. Zanim jednak zaczniemy je używać, warto dobrze poznać i zrozumieć podstawową funkcję `slice/3`. Dzięki temu łatwiej będzie nam zrozumieć działanie i zastosowanie pozostałych funkcji.

Warto także pamiętać, że funkcje do wydobywania podłańcuchów w Elixir są bardzo wydajne - w porównaniu do innych języków programowania, operacje na stringach w Elixir są znacznie szybsze i nie obciążają zasobów. Warto więc z nich korzystać, szczególnie w aplikacjach, które wymagają szybkiego przetwarzania dużej ilości tekstu.

## Zobacz także

- [Dokumentacja Elixir](https://hexdocs.pm/elixir/Kernel.String.html)
- [Książka "Programming Elixir"](https://pragprog.com/book/elixir/programming-elixir)