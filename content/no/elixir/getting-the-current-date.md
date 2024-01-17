---
title:                "Å få gjeldende dato"
html_title:           "Elixir: Å få gjeldende dato"
simple_title:         "Å få gjeldende dato"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/elixir/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Å få dagens dato i programmering refererer til å hente informasjon om den nåværende datoen fra ditt datamaskin eller server. Dette er en vanlig oppgave som programmerere må mestre, da det ofte er nødvendig for å håndtere tidsbaserte funksjoner, som å logge når en hendelse skjedde eller å planlegge gjentagende oppgaver.

## Hvordan:
Det er flere måter å få dagens dato i Elixir på, avhengig av hvilket nivå av nøyaktighet og formatering du trenger. Her er et eksempel på å hente dagens dato og tid med millisekunder:

```Elixir
DateTime.utc_now() |> DateTime.to_iso8601(~U)f
```

Output: "2021-03-20T14:31:24.123456Z"

Du kan også få dagens dato uten tid og med lokal tidssone med følgende kode:

```Elixir
Date.utc_today() |> Date.to_iso8601(~D)
```

Output: "2021-03-20"

## Dypdykk:
Å hente dagens dato er en viktig del av programmering siden det er nødvendig for mange tidsrelaterte funksjoner. Før moderne programmeringsspråk som Elixir ble utviklet, ble dato og tid håndtert gjennom komplekse datastrukturer som kalles epochs. Med Elixir og andre moderne språk blir dette prosessen enklere og mer intuitiv.

Det finnes også alternative måter å hente dagens dato på, som å bruke en tredjeparts bibliotek som Chronic for å håndtere datoer på en mer naturlig måte.

## Se Også:
- Offisiell Elixir dokumentasjon om dato:
https://hexdocs.pm/elixir/DateTime.html
- Chronic biblioteket:
https://github.com/akira/ex_chronic