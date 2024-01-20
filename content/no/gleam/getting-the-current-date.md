---
title:                "Få den gjeldende datoen"
html_title:           "Haskell: Få den gjeldende datoen"
simple_title:         "Få den gjeldende datoen"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/gleam/getting-the-current-date.md"
---

{{< edit_this_page >}}

# Få dagens dato med Gleam programmeringsspråk: En Rask Guide!

## Hva & Hvorfor?
Å få dagens dato betyr å hente den nåværende datoen fra systemets interne klokke. Dette er viktig for programmerere fordi det tillater å logge hendelser, sette tidfrister, måle tidsbruk, og mer.

## Hvordan til:
Dessverre, Gleam har ikke innebygd funksjonalitet for å få dagens dato ennå. Men, du kan bruke Erlang's 'date' funksjon:

``` Gleam
import erlang

fn current_date() {
  erlang.date()
}
```

Dette returnerer en tuple `(År, Måned, Dag)`. Du kan kjøre dette i Gleam's repl: 

``` Gleam
> current_date()
#(2022 1 4)
```

## Dypdykk
Å få dagens dato er en grunnleggende funksjon i de fleste programmeringsspråk, og Gleam er intet unntak selv om det delegerer denne funksjonen til Erlang. Selv om det er andre metoder for tidsmåling (f.eks. Unix tid), gir datoformatet et menneskelig leselig format og er lettere å forstå.

Implementeringen av 'date' funksjonen i Erlang er basert på Gregorian kalenderen, som er den kalenderen mest brukt internasjonalt for sivile formål. Den fungere ved å hente tidspunktet (i sekunder) fra det lokale operativsystemet og konverterer det til et datoformat (År, Måned, Dag).

## Se Også
For mer informasjon om Gleam og Erlang, besøk følgene kilder:
2. [Erlang 'date' funksjon dokumentasjon](http://erlang.org/doc/man/erlang.html#date-0)