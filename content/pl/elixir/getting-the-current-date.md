---
title:                "Pobieranie aktualnej daty"
date:                  2024-01-20T15:13:51.562349-07:00
html_title:           "Bash: Pobieranie aktualnej daty"
simple_title:         "Pobieranie aktualnej daty"

category:             "Elixir"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/elixir/getting-the-current-date.md"
---

{{< edit_this_page >}}

## What & Why? (Co i dlaczego?)
Pobieranie aktualnej daty to sposób na uzyskanie informacji o bieżącym dniu, miesiącu i roku. Programiści robią to by zapisywać znaczniki czasu, harmonogramować zadania i generować raporty.

## How to: (Jak to zrobić:)
Elixir używa modułu `DateTime` do pracy z datami. Oto jak łatwo uzyskać bieżącą datę:

```elixir
# Pobranie aktualnej daty i czasu w strefie UTC
{:ok, current_datetime} = DateTime.now("Etc/UTC")
IO.inspect(current_datetime)

# Jeśli interesuje nas tylko data
{:ok, date} = Date.today()
IO.inspect(date)
```

Na wyjściu zobaczysz coś w stylu:

```
# DateTime
%DateTime{
  year: 2023,
  month: 4,
  day: 5,
  ...
}

# Date
~D[2023-04-05]
```

## Deep Dive (Dogłębna analiza)
Elixir, język funkcyjny zrodzony w 2012 roku na fundamencie Erlanga, ma dobre wsparcie dla operacji na czasie. Moduł `DateTime` pochodzi z biblioteki standardowej, a Elixir korzysta z `Calendar` do obsługi różnych kalendarzy. Zanim powstał `DateTime`, programiści musieli polegać na zewnętrznych bibliotekach lub własnych implementacjach. Ważną rzeczą jest świadomość stref czasowych, co rozwiązuje moduł `Timezone`. Alternatywą dla wbudowanych modułów jest popularna biblioteka `Timex`, która oferuje więcej funkcji.

## See Also (Zobacz również)
- Dokumentacja Elixir `DateTime`: https://hexdocs.pm/elixir/DateTime.html
- Dokumentacja Elixir `Date`: https://hexdocs.pm/elixir/Date.html
- Timex, zaawansowana biblioteka do pracy z czasem: https://hexdocs.pm/timex/Timex.html
- Erlang/OTP, platforma dla Elixir: https://www.erlang.org/
