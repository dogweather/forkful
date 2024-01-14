---
title:                "Elixir: Å beregne en dato i fremtiden eller fortiden"
programming_language: "Elixir"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/elixir/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

I synet på kodingspråk er Elixir relativt nytt, men det har allerede fanget øyeblikkende til mange utviklere. Dette moderne språket, inspirert av funksjonell programmering, har blitt populært på grunn av sin utmerkede funksjonalitet og brukervennlighet. I denne blogginnlegget skal vi ta en titt på hvordan man kan beregne datoer i fortiden og fremtiden ved hjelp av Elixir.

## Hvorfor

Å beregne datoer i fortiden eller fremtiden kan være nyttig i forskjellige programmeringsscenarier. Det kan være alt fra å bestemme når et abonnement utløper, til å planlegge oppgaver eller aktiviteter for fremtiden. Uansett hva årsaken er, så vil utførelsen av dette utvilsomt være avhengerig av å ha en god forståelse av dateringssystemet til Elixir.

## Slik gjør du det

La oss begynne med et enkelt eksempel på hvordan vi kan beregne en dato i fremtiden ved hjelp av Elixir. Vi kan bruke funksjonen ```Date.add``` og spesifisere en dato og et antall dager å legge til. La oss si at vi ønsker å finne datoen 14 dager frem i tid fra i dag:

```Elixir
iex> Date.add(Date.utc_today(), 14)
#<Date<2018-09-28>>
```

Som du kan se, får vi en nøyaktig dato 14 dager frem i tid.

For å beregne en dato i fortiden, kan vi bruke funksjonen ```Date.subtract```. La oss si at vi ønsker å finne datoen 21 dager tilbake i tid fra i dag:

```Elixir
iex> Date.subtract(Date.utc_today(), 21)
#<Date<2018-08-20>>
```

Vi kan også legge til eller trekke fra måneder eller år ved å legge til en tredje parameter i funksjonene. For eksempel, hvis vi ønsker å finne datoen 6 måneder i fortiden fra i dag:

```Elixir
iex> Date.subtract(Date.utc_today(), 6, "months")
#<Date<2018-03-26>>
```

Det er også mulig å konvertere en tekststreng til en dato ved hjelp av funksjonen ```Date.from_iso8601```. Dette kan være nyttig hvis vi for eksempel ønsker å beregne datoen fra en brukers input. 

## Et dypdykk

For å utføre mer komplekse beregninger kan vi bruke funksjonen ```naive_datetime_to_erl_seconds``` for å konvertere datoer til en tidslinje som kan manipuleres. Dette gjør det mulig å beregne datoer basert på en gitt tidsperiode, for eksempel måneder eller år.

En annen nyttig funksjon er ```DateTime.to_rfc3339```, som gjør det mulig å konvertere datoer til et mer lesbart format.

Med disse funksjonene er det enklere å håndtere datorelaterete oppgaver i Elixir og sørge for nøyaktige resultater.

## Se også

For mer informasjon om hvordan man kan manipulere datoer i Elixir, sjekk ut følgende ressurser:

- [Elixir Date API dokumentasjon](https://hexdocs.pm/elixir/Date.html)
- [Offisiell Elixir nettside](https://elixir-lang.org/)
- [Elixir subreddit](https://www.reddit.com/r/elixir/)

Takk for at du leste denne bloggposten og vi håper den har gitt deg en bedre forståelse av hvordan man beregner datoer i Elixir!