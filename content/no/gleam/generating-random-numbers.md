---
title:                "Generering av tilfeldige tall"
html_title:           "Gleam: Generering av tilfeldige tall"
simple_title:         "Generering av tilfeldige tall"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/gleam/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?

Generering av tilfeldige tall er en vanlig oppgave for programmerere, og med god grunn. Tilfeldige tall brukes ofte for å legge til variasjon og utfordring i spill, simuleringer, og andre programmer. Det kan også være nyttig i sikkerhetstesting og kryptografiske applikasjoner.

## Hvordan:

Å generere tilfeldige tall i Gleam er enkelt med det innebygde Random biblioteket. Her er et eksempel på å generere et tilfeldig tall mellom 1 og 10:

```Gleam
import gleam/random

random.int(1, 10)
```

Dette vil resultere i et tilfeldig tall hver gang programmet kjøres, for eksempel: 8.

For å generere et tilfeldig desimaltall mellom 0 og 1, kan du bruke funksjonen `random.float()`.

## Dypdykk:

I programmering er det viktig å kunne generere tilfeldige tall pålitelig og effektivt. Tidligere ble pseudorandom tallgeneratorer (PRNGs) brukt til dette formålet, men de hadde en tendens til å følge forutsigbare mønstre og kunne ikke anses som helt tilfeldige. I dag brukes mer sofistikerte metoder som tilfeldige tallgeneratorer (TRNGs) som bruker faktorer som atmosfærisk støy eller subatomære partiklers bevegelse for å generere tilfeldighet.

Et alternativ til å bruke det innebygde Random biblioteket er å bruke en ekstern tjeneste som genererer tilfeldige tall. Dette kan være nyttig hvis du trenger å generere ekstremt store eller komplekse tilfeldige tall. Imidlertid bør du være forsiktig med å stole på eksterne tjenester for sikkerhetskritiske formål, da det kan føre til sårbarheter i koden din.

## Se også:

For mer informasjon om hvordan du kan bruke Random biblioteket i Gleam, kan du sjekke ut dokumentasjonen på [Gleam sin nettside](https://gleam.run/libraries/random/0.14.0/). Du kan også utforske andre funksjoner og metoder som tilbys i Random biblioteket for å generere tilfeldige tekststrenger eller tilfeldige datoer og tider.