---
title:    "Elixir: Sammenligning av to datoer"
keywords: ["Elixir"]
---

{{< edit_this_page >}}

## Hvorfor
Når man jobber med datoer i et programmeringsspråk, er det ofte nødvendig å sammenligne to datoer. Dette kan være for å finne ut hvilken dato som er tidligere eller senere, eller for å sjekke om de to datoene er like. Uansett hva årsaken er, vil det å kunne sammenligne datoer være en nyttig ferdighet å ha.

## Hvordan
For å sammenligne to datoer i Elixir, kan man bruke funksjonene `Date.compare/2` og `DateTime.compare/2`. Disse funksjonene sammenligner to datoer og returnerer et heltall basert på forholdet mellom dem. Et positivt tall betyr at den første datoen er senere enn den andre, et negativt tall betyr at den første datoen er tidligere enn den andre, og 0 betyr at de to datoene er like.

```Elixir
iex> Date.compare(~D[2020-05-01], ~D[2020-05-02])
1
iex> DateTime.compare(~U[2020-05-01 12:00:00], ~U[2020-04-30 12:00:00])
1
iex> Date.compare(~D[2020-01-01], ~D[2020-01-01])
0
```

En annen nyttig funksjon for å sammenligne datoer er `Date.before?/2`. Denne funksjonen returnerer `true` hvis den første datoen er tidligere enn den andre.

```Elixir
iex> Date.before?(~D[2020-01-01], ~D[2020-01-02])
true
iex> Date.before?(~D[2020-01-03], ~D[2020-01-02])
false
```

## Dypdykk
Når man sammenligner datoer i Elixir, må man være oppmerksom på at det finnes forskjellige typer datoer: date, time og datetime. Dette kan påvirke resultatet av sammenligningen. For eksempel, hvis man prøver å sammenligne en date og en datetime, vil resultatet alltid være at datetime er senere, uansett hva klokkeslettet er.

```Elixir
iex> Date.compare(~D[2020-01-01], ~U[2020-01-01 12:00:00])
-1
iex> DateTime.compare(~D[2020-01-01], ~D[2020-01-01])
1
```

Det er også viktig å merke seg at sammenligning av datoer også tar hensyn til år, måned og dag. Dette betyr at selv om to datoer har samme klokkeslett, men forskjellig år, vil de bli ansett som ulike datoer.

```Elixir
iex> Date.compare(~D[2020-01-01], ~D[2021-01-01])
-1
iex> DateTime.compare(~U[2020-01-01 12:00:00], ~U[2021-01-01 12:00:00])
-1
```

## Se også
- [Elixir Date module](https://hexdocs.pm/elixir/Date.html)
- [Elixir DateTime module](https://hexdocs.pm/elixir/DateTime.html)
- [Elixir Datetime tutorial](https://www.tutorialspoint.com/elixir/elixir_datetime_module.htm)