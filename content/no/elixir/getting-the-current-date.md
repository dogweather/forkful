---
title:                "Elixir: Å få gjeldende dato"
programming_language: "Elixir"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/elixir/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Hvorfor

De fleste programmer, uansett om de er store eller små, vil på et eller annet tidspunkt trenge å vite hva dagens dato er. Det kan være for å vise dagens dato til brukeren, for å lagre den i en database, eller for å beregne alderen til en person. Uansett hva grunnen er, er det viktig å vite hvordan man kan få tak i dagens dato i Elixir.

## Hvordan

Først og fremst må vi importere Elixir-modulen `Calendar` for å få tilgang til alle funksjonene som hjelper oss med å håndtere dato og tid. Deretter kan vi bruke funksjonen `Date.utc_today` for å få dagens dato i UTC-format. For eksempel:

```Elixir
iex> import Calendar

iex> Date.utc_today
~D[2020-03-24]
```

Hvis vi vil ha datoen i et annet format, for eksempel i den lokale tidssonen, kan vi bruke funksjonen `Date.today`. Denne vil returnere datoen i det formatet som er satt på datamaskinen vår. Her er et eksempel på hvordan vi kan få dagens dato i Oslo:

```Elixir
iex> Date.today("Europe/Oslo")
~D[2020-03-24]
```

Vi kan også bruke funksjonen `Calendar.strftime` for å få datoen på en spesifikk måte. Dette er spesielt nyttig når vi vil vise datoen til en bruker. For eksempel:

```Elixir
iex> Calendar.strftime(Date.utc_today, "%d-%m-%Y")
"24-03-2020"
```

## Dypdykk

Når vi bruker funksjonene `utc_today` og `today`, ser vi at datoen er representert som et Elixir struktur-objekt av typen `Date`. Dette objektet inneholder informasjon om dagen, måneden og året på en organisert måte, noe som gjør det enkelt å manipulere datoer. Vi kan bruke funksjoner som `Date.day`, `Date.month` og `Date.year` for å få de respektive verdiene.

I tillegg til å få tak i dagens dato, kan vi også få tak i tidspunktet med `Time.utc_now` og `Time.now`. Dette vil returnere tiden i UTC og i lokal tidssone, akkurat som funksjonene for datoen. Vi kan også bruke `Calendar.strftime` for å formatere tiden på en spesifikk måte.

## Se også

- [Calendar-dokumentasjon](https://hexdocs.pm/elixir/Calendar.html)
- [Elixir-dato og tid - en komplett guide](https://gorails.com/tutorials/elixir-dates-and-times)