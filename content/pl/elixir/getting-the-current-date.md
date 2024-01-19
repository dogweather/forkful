---
title:                "Pobieranie aktualnej daty"
html_title:           "Arduino: Pobieranie aktualnej daty"
simple_title:         "Pobieranie aktualnej daty"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/elixir/getting-the-current-date.md"
---

{{< edit_this_page >}}

##Co i Dlaczego?

Praca z bieżącą datą to jedno z wielu zadań, które programiści muszą wykonywać, umożliwiaj też śledzenie czasu, logowanie zdarzeń, generowanie raportów itp.

## Jak to zrobić:

By pobrac aktualną datę w Elixire, używamy modułu `DateTime`. Poniżej znajduje się kod, który zwroci dzisiejszą datę.

```elixir
DateTime.utc_now() |> DateTime.to_string()
```

Po uruchomieniu tego kodu, otrzymasz efekt podobny do tego:

```elixir
"2023-08-23T12:33:27.125681Z"
```

## Głębsze spojrzenie:

Mimo że jest wiele sposobów na odczytanie aktualnej daty w Elixir, `DateTime.utc_now()` jest najczęściej używanym. Ta funkcja zwraca bieżącą datę i czas w strefie czasowej UTC, co jest przydatne dla programistów, którzy pracują nad aplikacjami obsługującymi różne strefy czasowe.

Ale warto pamiętać, że Elixir ma również inne moduły do obsługi dat i czasu - jak `NaiveDateTime` i `Date`. `NaiveDateTime` obchodzi się bez strefy czasowej, co czyni go mniej precyzyjnym, ale prostszym w użyciu. W międzyczasie, `Date` obsługuje tylko daty, bez odniesienia do czasu.

## Zobacz również:

- Dokumentacja Elixira na temat DateTime: https://hexdocs.pm/elixir/DateTime.html
- Dokumentacja Elixira na temat Date: https://hexdocs.pm/elixir/Date.html
- Dokumentacja Elixira na temat NaiveDateTime: https://hexdocs.pm/elixir/NaiveDateTime.html