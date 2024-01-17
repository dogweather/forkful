---
title:                "Sammenligning av to datoer"
html_title:           "Elixir: Sammenligning av to datoer"
simple_title:         "Sammenligning av to datoer"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/elixir/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Hva og hvorfor?
I programmering kan det være nyttig å sammenligne to datoer, for eksempel når du jobber med tidsserier eller ønsker å kontrollere om en hendelse skjedde før eller etter en annen hendelse. Dette er spesielt viktig når du jobber med store mengder data og trenger å finne spesifikke tidsintervaller.

## Hvordan:
Elixir tilbyr innebygd støtte for å sammenligne to datoer ved hjelp av ```:calendar```-modulen. Vi kan bruke funksjonen ```DateTime.compare/2``` for å sammenligne to datoer og få tilbake en av tre verdier: 1 hvis første dato er større, 0 hvis begge datoene er like, og -1 hvis første dato er mindre.

```Elixir
date1 = {{2019, 10, 25}, {12, 30, 00}}
date2 = {{2019, 10, 24}, {11, 00, 00}}

DateTime.compare(date1, date2)
# => 1
```
Denne funksjonen er nyttig hvis vi ønsker å sammenligne både dato og tid. Hvis vi bare vil sammenligne datoer, kan vi bruke funksjonen ```Date.compare/2``` som tar inn to ```Date```-strukturer og returnerer samme type svar.

```Elixir
d1 = ~D[2019-10-25]
d2 = ~D[2019-10-24]

Date.compare(d1, d2)
# => 1
```

## Dypdykk:
Datoer har vært en viktig del av dataprogrammering siden starten. Tidligere var dato og tid bare representert som tall, og det var opp til programmøren å håndtere konvertering og beregninger. I dag tilbyr moderne språk som Elixir innebygd støtte for datoer, noe som gjør det enklere og mer nøyaktig å håndtere dem. Alternativet til å bruke funksjoner i ```:calendar```-modulen er å benytte seg av tredjeparts biblioteker som ```timex``` eller ```calendar``` for mer avansert funksjonalitet. Men for de fleste tilfeller vil de innebygde funksjonene være tilstrekkelig.

## Se også:
- [Elixir's official documentation on ```:calendar``` module](https://hexdocs.pm/elixir/Calendar.html)
- [timex](https://hexdocs.pm/timex)
- [calendar](https://hexdocs.pm/calendar)