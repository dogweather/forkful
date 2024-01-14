---
title:                "Elixir: Få gjeldende dato"
simple_title:         "Få gjeldende dato"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/elixir/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Hvorfor

Få gjeldende dato er en vanlig oppgave når du utvikler programmer, spesielt hvis du jobber med tidssensitive applikasjoner eller logger. Elixir gir en enkel måte å få tak i dagens dato på, og i denne bloggposten vil jeg vise deg hvordan.

## Hvordan

For å få dagens dato i Elixir, kan du bruke funksjonen `Date.utc_today()`. Denne funksjonen returnerer i tillegg til dagens dato, også dagens tidssone og tid i UTC. La oss se på et eksempel:

```Elixir
iex> Date.utc_today()
{:ok, ~U[2021-10-05 16:33:09Z]}
```

Som du kan se, returneres resultatet som en tuple med to elementer. Det første elementet er `:ok`, som er en indikasjon på at funksjonen kjørte uten problemer. Det andre elementet er selve datoen i UTC-format.

Hvis du bare er interessert i dagens dato uten tid og tidssone, kan du bruke funksjonen `Date.utc_today |> Date.to_iso8601`. Dette vil returnere datoen i ISO 8601-format, som vanligvis er lettere å lese og behandle.

```Elixir
iex> Date.utc_today() |> Date.to_iso8601()
{:ok, "2021-10-05"}
```

Du kan også spesifisere en spesifikk tidssone ved å legge til en parameter i `Date.utc_today()`-funksjonen. For eksempel:

```Elixir
iex> Date.utc_today("Europe/Oslo") |> Date.to_iso8601()
{:ok, "2021-10-05"}
```

Dette vil returnere datoen i Oslo sin tidssone.

## Dypdykk

Bak kulissene bruker Elixir funksjonen `:calendar.universal_time()` for å få dagens dato og tid i UTC-format. Denne funksjonen returnerer en tuple med dato og tid i UTC-format. Funksjonen `Date.to_iso8601()` som vi brukte i eksemplene våre, konverterer denne tuplen til en enkel streng.

En annen måte å få dagens dato på er ved å bruke den innebygde Elixir-modulen `:os.date()`. Denne funksjonen returnerer datoen og tid i lokalt format. Hvis du ønsker å endre formatet, kan du spesifisere det som en parameter. For eksempel:

```Elixir
iex> :os.date("Dato: %d.%m.%Y, Klokken: %H:%M:%S")
"Dato: 05.10.2021, Klokken: 18:52:45"
```

Dette vil returnere datoen og tiden i et spesifisert format.

## Se også

- [Elixir Date-modulen](https://hexdocs.pm/elixir/Date.html)
- [ISO 8601-datoformat](https://en.wikipedia.org/wiki/ISO_8601)