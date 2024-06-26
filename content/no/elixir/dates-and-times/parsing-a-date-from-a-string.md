---
changelog:
- 2024-01-28, dogweather, reviewed
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 02:05:06.262105-07:00
description: "Hvordan: I Elixir kan du parse datoer ved \xE5 bruke `Date`-modulen.\
  \ Slik gj\xF8r du en streng om til en dato."
lastmod: '2024-03-13T22:44:40.452880-06:00'
model: gpt-4-0125-preview
summary: "I Elixir kan du parse datoer ved \xE5 bruke `Date`-modulen."
title: Analysering av en dato fra en streng
weight: 30
---

## Hvordan:
I Elixir kan du parse datoer ved å bruke `Date`-modulen. Slik gjør du en streng om til en dato:

```elixir
date_string = "2023-04-05"
{:ok, date} = Date.from_iso8601(date_string)
IO.inspect(date)
```

Eksempel på utdata:

```elixir
~D[2023-04-05]
```

For å håndtere forskjellige formater, kan du bruke `Timex`-biblioteket:

```elixir
{:ok, datetime} = Timex.parse("05-04-2023", "{D}-{0M}-{YYYY}")
IO.inspect(datetime)
```

Eksempel på utdata:

```elixir
#DateTime<2023-04-05 00:00:00Z>
```

## Dypdykk
Funksjonen `Date.from_iso8601/1` er en del av Elxirs standardbibliotek, introdusert for å sikre enkel parsing av ISO8601-datostandarden - et vanlig datoformat. Men livet er ikke så enkelt; datoer kommer i tonnevis av formater. Det er her `Timex`, et tredjeparts Elixir-bibliotek, kommer inn i bildet. Det er rikere enn de innebygde Elixir-dato funksjonene og hjelper med å håndtere et bredt spekter av datoformater.

Elixir selv er uforanderlig, noe som betyr at analyserte datoer ikke er et unntak; de kan ikke endres etter de er opprettet. Denne funksjonen knytter tilbake til de funksjonelle programmeringsrøttene til Elixir, og garanterer forutsigbarhet og enklere feilsøking.

Historisk sett har datoparsing vært vanskelig på grunn av varierte standarder. Men med biblioteker som `Timex` og språkegenskaper i Elixir, blir kompleksiteten abstrahert bort, noe som gjør livet til en utvikler litt enklere.

## Se Også
- [Elixir Date](https://hexdocs.pm/elixir/Date.html)
- [Timex Documentation](https://hexdocs.pm/timex/Timex.html)
- [ISO8601 Standard](https://www.iso.org/iso-8601-date-and-time-format.html)
