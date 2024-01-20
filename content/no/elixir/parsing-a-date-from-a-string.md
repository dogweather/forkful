---
title:                "Tolke en dato fra en streng"
html_title:           "Bash: Tolke en dato fra en streng"
simple_title:         "Tolke en dato fra en streng"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/elixir/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

# Informell Innføring i Parsing av Dato i Elixir

## Hva & Hvorfor?

Parsing av dato fra en tekststreng er prosessen der vi konverterer en streng som representerer en dato, til en faktisk datatypen. Dette er viktig for at programmet skal behandle datorelaterte operasjoner, som sammenligninger, beregninger osv, korrekt.

## Hvordan:

Vi skal benytte `Date.from_iso8601`-funksjonen i Elixir til å parse strenge. Her er en prøvekode:

```elixir
    {:ok, date} = Date.from_iso8601("2023-11-15")
```

Ved kjøring av ovenstående kode vil `date` variabelen være Elixir-datoobjektet som representerer 15. november 2023.

## Dypdykk

I tidligere programmeringsspråk, var streng til dato-parsing en flertrinnsprosess som krevde mye kode. Med Elixirs innebygde funksjoner, har det blitt en enkeltlinjeprosess. Elixir-parse funksjoner er svært robuste, og tilgir mange vanlige feil i inputformatet.

Som et alternativ til `Date.from_iso8601`, kan du også benytte `Date.parse` som gir mer fleksible datoformatalternativer.

På implementeringsnivå leser `Date.from_iso8601` først året, måneden og dagen fra strengen og sjekker deretter om disse verdiane utgjør en gyldig dato eller ikke.

## Se Også:

[Elixir School: Dates](https://elixirschool.com/en/lessons/basics/date/) - En ressurs dedikert til Elixirs dato-konsepter, inkludert parsing.

[Elixir Documentation: Date from_iso8601](https://hexdocs.pm/elixir/Date.html#from_iso8601/1) - Direkte fra kildens dokumentasjon på `Date.from_iso8601`.