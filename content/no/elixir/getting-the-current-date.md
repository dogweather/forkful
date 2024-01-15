---
title:                "Få den nåværende datoen"
html_title:           "Elixir: Få den nåværende datoen"
simple_title:         "Få den nåværende datoen"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/elixir/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Hvorfor
Å få den nåværende datoen kan være en svært nyttig funksjon i ethvert programmeringsprosjekt. Det kan hjelpe deg med å spore og organisere data, eller vise brukerne nøyaktig når noe ble opprettet eller endret.

## Hvordan
```Elixir
Date.utc_today() 
# => ~U[2019:03:21 00:00:00Z]
```
Den enkleste måten å få den nåværende datoen i Elixir er å bruke funksjonen `Date.utc_today()`. Dette vil returnere datoen i UTC-format. Hvis du ønsker å endre tidssonen til datoen, kan du bruke funksjonen `Date.from_erl({{year, month, day}, {hour, min, sec}})` og lagre den i en variabel. Deretter kan du bruke `Date.utc_today() |> Date.to_erl()` for å få datoen i ønsket tidssone.

## Deep Dive
I Elixir er datoen representert som en struct (en sammensatt datastruktur) som består av attributtene `year`, `month` og `day`. Dette gjør det enkelt å manipulere datoer ved å bruke funksjoner som `Date.add/2` og `Date.sub/2` for å legge til eller trekke fra en bestemt tidsperiode. Det er også en rekke innebygde funksjoner for å hente ut informasjon som ukedag, kalenderuke og år.

## Se Også
- [Elixir Date Module Documentation](https://hexdocs.pm/elixir/Date.html)
- [Elixir DateTime Module Documentation](https://hexdocs.pm/elixir/DateTime.html)
- [Elixir Timezone Database Library](https://hexdocs.pm/tzdata/readme.html)