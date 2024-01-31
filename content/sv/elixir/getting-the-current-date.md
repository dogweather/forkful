---
title:                "Att hämta aktuellt datum"
date:                  2024-01-20T15:14:02.745154-07:00
html_title:           "Bash: Att hämta aktuellt datum"
simple_title:         "Att hämta aktuellt datum"

category:             "Elixir"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/elixir/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att få fram det aktuella datumet innebär att vi hämtar information om nuvarande dag, månad och år. Programmerare gör detta för att hantera tidsrelaterade data, som utgångsdatum eller loggning av händelser.

## Så Här Gör Du:
I Elixir, använd modulen `Date` för att få fram dagens datum:

```elixir
date = Date.utc_today()
IO.inspect(date)
```

Exempel på utskrift:

```elixir
~D[2023-04-05]
```

För att formatera datumet enligt egna behov, använd `Date.to_string/1`:

```elixir
formatted_date = Date.utc_today() |> Date.to_string()
IO.puts(formatted_date)
```

Exempel på utskrift:

```elixir
"2023-04-05"
```

## Djupdykning
Elixir använder `Date`, `Time`, och `DateTime` moduler för att hantera datum och tid. Dessa moduler är en del av den inbyggda `Elixir` standardbiblioteket och introducerades i Elixir 1.3 för att ge en robust hantering av datum och tider. 

Det finns flera sätt att hantera datum och tider i Elixir. Utöver `Date.utc_today/0`:

- `DateTime.utc_now/0` ger dig den aktuella tiden och datumet i UTC.
- `NaiveDateTime.local_now/0` för tid utan tidszon.
- Använd `Timex` biblioteket för mer komplexa datum/tidsfunktioner.

Implementationen av datumbearbetning i Elixir lutar sig mot Erlang's kalendermoduler, men erbjuder en mer Elixir-vänlig syntax och ytterligare funktioner.

## Se Även
- Elixir's officiella dokumentation för Date-modulen: https://hexdocs.pm/elixir/Date.html
- `Timex`, ett populärt tredjepartstidsbibliotek för Elixir: https://hexdocs.pm/timex/Timex.html
- Erlang's dokumentation om kalendermodulen: http://erlang.org/doc/man/calendar.html
