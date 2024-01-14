---
title:    "Elixir: Få gjeldende dato"
keywords: ["Elixir"]
---

{{< edit_this_page >}}

## Hvorfor

Det er viktig å være i stand til å få den nåværende datoen i Elixir-programmering for å kunne lage dynamiske og aktuelle applikasjoner. Datoen brukes ofte til å vise relevant informasjon til brukeren, eller for å utføre handlinger basert på nåværende dato.

## Slik gjør du det

For å få den nåværende datoen i Elixir, kan vi bruke funksjonen `:calendar.local_time`, som returnerer en liste med informasjon om gjeldende datoen, tiden og tidssonen. Her er et eksempel:

```
Elixir IEx> :calendar.local_time
{:ok, {{2019, 8, 27}, {16, 35, 46}}, :local}
```

Vi kan også bruke funksjonen `:calendar.universal_time`, som returnerer en lignende liste, men med tidssonen satt til `:utc`.

```
Elixir IEx> :calendar.universal_time
{:ok, {{2019, 8, 27}, {14, 35, 46}}, :utc}
```

Vi kan også bruke funksjonen `:calendar.universal_time_to_local_time/1` for å konvertere tidssonen fra UTC til lokal tid.

```
Elixir IEx> :calendar.universal_time_to_local_time(:utc)
#--- Resultat: {{2019, 8, 27}, {16, 35, 46}}
```

## Dypdykk

Nå som vi har fått tak i den nåværende datoen og tiden, kan vi også utføre ulike operasjoner på den ved hjelp av Elixir-funksjoner. For eksempel kan vi bruke `:calendar.date_diff/2` for å finne antall dager mellom to datoer.

```
Elixir IEx> :calendar.date_diff({2019,8,29}, {2019,8,27})
2
```

Vi kan også bruke funksjoner som `:calendar.is_leap_year/1`, `:calendar.is_dst/1` og `:calendar.valid_date?/3` for å sjekke ulike aspekter av den nåværende datoen.

## Se også

- Offisiell Elixir dokumentasjon for `:calendar` modulen: https://hexdocs.pm/elixir/Calendar.html
- Elixir Forum diskusjon om å få den nåværende datoen: https://elixirforum.com/t/get-current-date-or-now/6021/2