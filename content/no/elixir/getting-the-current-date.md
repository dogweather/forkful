---
title:                "Slik får du tak i dagens dato"
date:                  2024-01-20T15:14:02.082285-07:00
html_title:           "C: Slik får du tak i dagens dato"
simple_title:         "Slik får du tak i dagens dato"

category:             "Elixir"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/elixir/getting-the-current-date.md"
---

{{< edit_this_page >}}

## What & Why?
Få dagens dato: ganske enkelt, det betyr å hente dagens kalenderdato fra systemet. Programmerere trenger dette for å timestamp data, håndtere oppgaver, og for tidssensitive applikasjoner.

## How to:
Elixir gjør det enkelt å få dagens dato med `Date` modulen:

```elixir
date = Date.utc_today()
IO.inspect(date)
```

Output:

```elixir
~D[2023-04-12]
```

For å inkludere tid, bruk `DateTime`:

```elixir
datetime = DateTime.utc_now()
IO.inspect(datetime)
```

Output:

```elixir
#DateTime<2023-04-12 09:00:00Z>
```

## Deep Dive
I Elixir, introduserte `Date` og `DateTime` med versjon 1.3 for å støtte dato- og tidshåndtering. Dette var en del av en større innsats for å inkludere mer robuste standardbiblioteker.

Det finnes alternativer også. For eksempel, biblioteket `Timex` gir mer funksjonalitet rundt dato- og tidshåndtering hvis du trenger det.

Under hetten konverterer Elixir systemtid til en mer håndterlig format, `"Calendar"`. `Calendar.ISO` er standardkalendermodulen som gir oss `Date` og `DateTime`.

## See Also
- Elixir's offisielle dokumentasjon for `Date`: [https://hexdocs.pm/elixir/Date.html](https://hexdocs.pm/elixir/Date.html)
- Elixir's offisielle dokumentasjon for `DateTime`: [https://hexdocs.pm/elixir/DateTime.html](https://hexdocs.pm/elixir/DateTime.html)
- Timex, en rik tredjepartsbibliotek: [https://hexdocs.pm/timex/Timex.html](https://hexdocs.pm/timex/Timex.html)
- Internett UTC tid: [https://www.timeanddate.com/worldclock/timezone/utc](https://www.timeanddate.com/worldclock/timezone/utc)
