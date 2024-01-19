---
title:                "Sammenligner to datoer"
html_title:           "Clojure: Sammenligner to datoer"
simple_title:         "Sammenligner to datoer"
programming_language: "Elm"
category:             "Elm"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/elm/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?

Sammenligning av to datoer er prosessen med å beregne forskjellen mellom dem for å bestemme hvilken som kommer først eller sist. Programmerere gjør dette for å utføre handlinger basert på tid, slik som planlegging av oppgaver eller sortering av hendelser.

## Hvordan:

Her er en enkel måte å sammenligne datoer på i Elm:

```Elm
import Time 

dateComparison : Time.Posix -> Time.Posix -> String
dateComparison date1 date2 =
    if Time.millisToPosix date1 < Time.millisToPosix date2 then
        "Date1 er tidligere enn Date2"
    else if Time.millisToPosix date1 > Time.millisToPosix date2 then
        "Date1 er senere enn Date2"
    else
        "Datoene er like"
```
Her konverterer vi to datoer (date1 og date2) til millisekunder siden "Unix-epoken", og sammenligner millisekundene. Utfallet kan være at den første datoen er tidligere enn, senere enn, eller lik den andre datoen.

## Dypdykk:

Sammenligning av datoer har vært en enkel, men viktig del av dataprogrammering siden de tidlige dagene. I Elm, bruker vi `Time.Posix` for å håndtere dato og tid. Dette er basert på Unix-tid, en metode for å spore tid som begynte 1. januar 1970.

Alternativt kan vi også bruke biblioteket `elm-date-extra`, som gir flere funksjoner for å arbeide med datoer.

Dypere detaljer om implementeringen kan variere avhengig av dine spesifikke behov, inkludert håndtering av tidssoner og sommertid.

## Se også:

For mer informasjon, sjekk ut disse nyttige lenkene:

- Elm Time-modul dokumentasjon: https://package.elm-lang.org/packages/elm/time/latest/
- Elm Date-Extra bibliotek: https://package.elm-lang.org/packages/rluiten/elm-date-extra/latest
- Unix-tid: https://no.wikipedia.org/wiki/Unix-tid