---
title:    "Elixir: Sammenligning av to datoer"
keywords: ["Elixir"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/no/elixir/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Hvorfor

Når du jobber med Elixir programmering, kan det være nyttig å kunne sammenligne to datoer. Dette kan hjelpe deg med å organisere og sortere data på en effektiv måte. I denne bloggposten skal vi gå gjennom hvordan du kan sammenligne to datoer i Elixir.

## Hvordan

Det finnes flere forskjellige måter å sammenligne datoer på i Elixir, avhengig av hva du prøver å oppnå. La oss gå gjennom noen eksempler og se på resultatene.

```Elixir
date1 = ~D[2021-01-01]
date2 = ~D[2020-01-01]
date3 = ~D[2021-01-01]

date1 > date2 # true
date1 < date3 # false
```

Her ser vi hvordan vi kan bruke operatørene ">", "<" og "=" for å sammenligne datoer. Det er viktig å merke seg at datoene må være i form av ElixirDate structs, som vi har definert ved hjelp av "~D" syntaksen. 

En annen måte å sammenligne datoer på er ved bruk av funksjonen `DateTime.compare/2`. Denne funksjonen returnerer en numerisk verdi som indikerer forholdet mellom to datoer. 

```Elixir
date1 = ~D[2021-01-01]
date2 = ~D[2020-01-01]
date3 = ~D[2021-01-01]

DateTime.compare(date1, date2) # 1
DateTime.compare(date1, date3) # 0
```

I dette tilfellet vil `DateTime.compare` returnere en verdi på 1 hvis `date1` er senere enn `date2`, 0 hvis de to datoene er like, og -1 hvis `date1` er tidligere enn `date2`.

## Dypdykk

Når du sammenligner to datoer, er det viktig å være oppmerksom på hvordan datoene blir lagret og behandlet. I Elixir er datoer basert på "Gregorian Calendar", som er den mest brukte kalenderen internasjonalt. Dette betyr at datoene blir konvertert og behandlet i henhold til denne kalenderen, uavhengig av hvilken kalender som brukes i ditt land.

Det er også viktig å merke seg at datoer er immutable i Elixir. Dette betyr at når en dato er opprettet, kan den ikke endres. Dette betyr at alle sammenligninger du gjør med datoer ikke vil endre de opprinnelige datoene, men heller returnere en ny verdi.

## Se også

- Offisiell Elixir dokumentasjon: https://hexdocs.pm/elixir/Date.html
- Sammenligning av datoer i Elixir forumt innlegg: https://elixirforum.com/t/date-compare/2491
- Elixir forumt tråd om variasjoner i datoer: https://elixirforum.com/t/different-date-conversions/4850/5