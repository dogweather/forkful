---
title:    "Elixir: Beregning av dato i fremtiden eller fortiden"
keywords: ["Elixir"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/no/elixir/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

# Hvorfor

Det hender ofte at man trenger å beregne en dato som ligger enten i fremtiden eller fortiden. Dette kan være nyttig i programmering for å planlegge hendelser eller håndtere datoer i en applikasjon. Med Elixir kan man enkelt utføre slike beregninger.

# Hvordan

For å beregne en dato i fremtiden eller fortiden i Elixir, kan man bruke funksjonen `Timex.shift/4`. Denne funksjonen tar inn fire argumenter: `timestamp`, `shift`, `unit` og `timezone`. Her er et eksempel på hvordan man kan bruke denne funksjonen for å beregne en dato 30 dager frem i tid:

```Elixir
iex> Timex.shift(~N[2020-10-10 12:00:00], 30, :days)
~N[2020-11-09 12:00:00]
```

Man kan også bruke funksjonen til å beregne en dato i fortiden ved å bruke et negativt tall som `shift`. For eksempel, for å beregne en dato 2 år, 6 måneder og 10 dager tilbake i tid:

```Elixir
iex> Timex.shift(~N[2020-10-10 12:00:00], -2, :years, -6, :months, -10, :days)
~N[2018-03-00 12:00:00] # February har bare 28 dager, så det vises som "00"
```

# Dypdykk

Når man bruker `Timex.shift/4` funksjonen, blir `timestamp` argumentet internasjonalt standardisert som UTC. Derfor, om man ønsker å hente ut resultatet i en annen tidssone, må man spesifisere det ved å bruke `timezone` argumentet. For eksempel, for å beregne en dato i den amerikanske østkysten tidssone:

```Elixir
iex> Timex.shift(~N[2020-10-10 12:00:00], 10, :hours, "America/New_York")
~N[2020-10-10 22:00:00]
```

Det er også mulig å bruke mer presise enheter for `shift` argumentet, som for eksempel `:weeks`, `:hours` eller `:minutes`. Man kan også kombinere enheter og bruke `Timex.Interval` modulen for mer kompliserte beregninger.

# Se Også

- Offisiell Elixir dokumentasjon for [Timex](https://hexdocs.pm/timex/Timex.html#shift/4)
- [Kalkulere datoer på en enkel måte i Elixir](https://h3rald.com/articles/2016/10/18/calculate-dates-the-easy-way-in-elixir/) av Øystein Dørum
- Elixir [cheat sheet](https://elixirschool.com/lessons/basics/cheatsheet/) fra Elixir School