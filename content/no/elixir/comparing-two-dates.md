---
title:                "Sammenligner to datoer"
html_title:           "Clojure: Sammenligner to datoer"
simple_title:         "Sammenligner to datoer"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/elixir/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?

Sammenligning av to datoer innebærer å sjekke om en dato er tidligere, senere eller samtidig som en annen dato. Programmerere bruker dette for å håndtere begivenheters rekkefølge og tidspent i applikasjoner. 

## Hvordan Gjøre Det:

Her vil vi se på hvordan du kan sammenligne to datoer i Elixir. 

```elixir
d1 = Date.new(2020, 12, 12)
d2 = Date.new(2021, 12, 12)

IO.puts Date.compare(d1, d2)
```
Outputten vil være `:lt`, hvilket betyr at d1 er mindre enn d2 (eller d1 kommer før d2).

```elixir
d3 = Date.new(2022, 12, 12)

IO.puts Date.compare(d2, d3)
```
Outputten vil være `:lt`, siden d2 er mindre enn d3. 

```elixir
d4 = Date.new(2022, 12, 12)

IO.puts Date.compare(d3, d4)
```
Her vil outputten være `:eq`, siden d3 er lik d4.

## Dypdykk:

Historisk sett hadde ikke Elixir innebygget støtte for datoer, og sammenligning av datoer var ikke så greit. Man måtte ty til biblioteker som Timex for å få dette gjort. 

Elixir har nylig fått bedre innebygd støtte for datoer. Fra versjon 1.3 har Elixir `Date.compare/2` -funksjonen som gir en enkel måte å sammenligne datoer på. 

Alternativt kan du bruker operatoren `>`,`<` og `==` når du sammenligner to `DateTime` strukturer. Men, holdt i tankene at disse operatorene ikke vil fungere når du sammenligner to `Date` strukturer.

En viktig ting å merke seg ved `Date.compare/2` -funksjonen er at den tar i betraktning datoen **kun**, og ignorerer tidsdelen i `DateTime` strukturer.

## Se Også:

For mer informasjon, sjekk ut disse lenkene: 

- Erlangs offisielle datodokumentasjon: [https://erlang.org/doc/man/calendar.html]
- Elixir's offisielle Date-module dokumentasjon: [https://hexdocs.pm/elixir/Date.html]
- Ulike metoder for å håndtere datoer og tid i Elixir: [https://dev.to/jonlunsford/working-with-dates-and-times-in-elixir-4p3d]