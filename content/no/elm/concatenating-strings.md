---
title:                "Elm: Sammenstilling av strenger"
programming_language: "Elm"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/elm/concatenating-strings.md"
---

{{< edit_this_page >}}

# Hvorfor
Velkommen til Elm programmeringsbloggen! I dag skal vi snakke om hvorfor det er viktig å kunne legge sammen strenger i Elm. Å kunne legge sammen strenger er en grunnleggende ferdighet som vil hjelpe deg med å bygge mer komplekse og dynamiske applikasjoner i Elm. 

# Slik gjør du det
Så hvordan legger man sammen strenger i Elm? Det er faktisk ganske enkelt ved hjelp av den innebygde funksjonen `String.join`. Denne funksjonen tar inn en liste av strenger og en streng som brukes som lim mellom hver streng i listen. La oss se på et eksempel:

```Elm
navn = "Ola"
bosted = "Oslo"

hilsen = String.join " fra " [navn, bosted]
```

I dette tilfellet vil variabelen `hilsen` være lik "Ola fra Oslo". Som du ser, så er det enkelt å legge sammen strenger ved hjelp av `String.join`. La oss se på et annet eksempel der vi ønsker å lage en melding som gir informasjon om alder:

```Elm
alder = 27

melding = "Jeg er " ++ String.fromInt alder ++ " år gammel"
```

Her bruker vi operatoren `++` til å legge sammen tekststrenger og funksjonen `String.fromInt` for å konvertere alderen til en streng. Variabelen `melding` vil da være lik "Jeg er 27 år gammel".

# Dypdykk
Å kunne legge sammen strenger på en effektiv måte er viktig for å kunne bygge avanserte applikasjoner i Elm. Det kan være nyttig å også kunne bruke funksjonen `String.concat` og operatorene `++` og `<<` for å legge sammen flere strenger eller variabler. Du kan også bruke den innebygde funksjonen `String.repeat` for å gjenta en streng et gitt antall ganger. 

# Se også
- [Elm dokumentasjon om strenger](https://package.elm-lang.org/packages/elm/core/latest/String)
- [Interaktiv Elm strengkalkulator](https://elm-lang.org/examples/string-calculator)
- [Elm offisiell nettside](https://elm-lang.org/)