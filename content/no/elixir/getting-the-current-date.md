---
title:    "Elixir: Å få nåværende dato"
keywords: ["Elixir"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/no/elixir/getting-the-current-date.md"
---

{{< edit_this_page >}}

# Hvorfor bruke Elixir for å få nåværende dato?

Det å kunne få nåværende dato er en viktig funksjon i mange programmeringsoppgaver. Enten du oppretter en kalenderapplikasjon, håndterer bestillinger, eller bare trenger å vite dagens dato, er det nødvendig å kunne få tilgang til denne informasjonen. Med Elixir, en funksjonell programmeringsspråk designet for å være robust og distribuert, er det enkelt å få nåværende dato ved hjelp av noen få linjer med kode.

# Hvordan få nåværende dato i Elixir

For å få nåværende dato i Elixir, kan du bruke funksjonen `Date.utc_today`, som returnerer en `Date` struktur som inneholder informasjon om nåværende dato, i UTC-tidssone.

```Elixir
Date.utc_today()

%Date{day: 17, month: 6, year: 2021}
```

Om du ønsker å få nåværende dato i din lokale tidssone, kan du bruke `Date.utc_today |> Date.to_erl |> :calendar.universal_time_to_local_time`.

```Elixir
Date.utc_today |> Date.to_erl |> :calendar.universal_time_to_local_time()

%{calendar: Calendar.ISO, day: 17, hour: 9, microsecond: {0,0,0}, minute: 0, month: 6, second: 0, standard_offset: 0, time_zone: "Europe/Oslo", universal_time: %{calendar: Calendar.ISO, day: 17, hour: 7, microsecond: {0,0,0}, minute: 0, month: 6, second: 0, time_zone: "Etc/UTC", universal_time: {2021,6,17}}}
```

Som du kan se i eksemplene over, gir `Date.utc_today` forskjellige utdata avhengig av om du ønsker å bruke UTC-tidssone eller din lokale tidssone. Dette skyldes at `Date` strukturer inkluderer informasjon om tidssoner.

# Dypdykk i å få nåværende dato i Elixir

Bak kulissene bruker `Date.utc_today` funksjonen `System.get_time`, som returnerer nåværende tid i millisekunder siden 1. januar 1970. Denne informasjonen blir deretter konvertert til en `Date` struktur ved hjelp av `Date.from_erl` funksjonen. `Date` strukturer inkluderer også funksjoner for å utføre forskjellige operasjoner, som å hente ut informasjon om en spesifikk dato eller sammenligne datoer.

Se også
- [Elixir Dokumentasjon om Date modulen](https://hexdocs.pm/elixir/Date.html)
- [Elixir Dokumentasjon om System modulen](https://hexdocs.pm/elixir/System.html)