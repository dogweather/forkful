---
title:    "Elm: Utskrift av feilsøkingsutdata"
keywords: ["Elm"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/no/elm/printing-debug-output.md"
---

{{< edit_this_page >}}

# Hvorfor bruke print i Elm programmering

Mange utviklere foretrekker å bruke print-funksjonen når de programmerer, spesielt under utviklingsprosessen. Det er viktig å forstå hvorfor dette er nyttig og hvordan man kan implementere det i Elm.

## Hvordan bruke print i Elm
For å bruke print-funksjonen i Elm kan du bruke "Debug.log" funksjonen. Dette gjør det mulig å skrive ut verdier og variabler direkte til Elm sin Developer Console. Her er et eksempel på hvordan dette kan gjøres:

``` Elm
main = 
    let
        name = "Elm"
        age = 5
    in
        Debug.log "Dette er navnet" name
        Debug.log "Dette er alderen" (toString age)
```

Outputen vil se slik ut i konsollen:
``` Elm
"Dette er navnet": "Elm"
"Dette er alderen": "5"
```

Dette kan være spesielt nyttig når man prøver å feilsøke koden sin eller for å se verdiene til variablene på et gitt tidspunkt i programmet.

## Dypdykk i print-funksjonen
En annen nyttig ting med print-funksjonen er at den kan brukes til å skrive ut verdier i løkker eller funksjoner. Dette kan hjelpe deg med å forstå hvordan koden din fungerer under forskjellige forhold. En annen måte å bruke det på er å printe ut verdier i HTTP-requests for å se hvilke data som blir hentet.

Det er også verdt å merke seg at print-funksjonen kun vil bli kjørt under utvikling og ikke i produksjon. Dette betyr at koden din ikke blir påvirket av å ha print-linjer i den når du publiserer applikasjonen din.

# Se også
- [Offisiell Elm dokumentasjon](https://guide.elm-lang.org/)
- [Elm forum](https://discourse.elm-lang.org/)
- [Elm slack samfunn](https://elmlang.herokuapp.com/)