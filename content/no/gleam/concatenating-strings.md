---
title:    "Gleam: Sammenslåing av strenger"
keywords: ["Gleam"]
---

{{< edit_this_page >}}

## Hvorfor
Å kombinere strenger, eller å legge til flere strenger sammen, er en grunnleggende ferdighet i ethvert programmeringsspråk. Dette er nyttig for å lage mer komplekse utsetninger eller sette sammen data for å vise en fullstendig melding. Det er også en viktig del av å jobbe med datastrukturer og manipulere informasjon.

## Hvordan gjøre det
For å kombinere strenger i Gleam, bruker vi operatøren `++`. Her er et eksempel på hvordan dette kan se ut i praksis:

```Gleam
let navn = "Siri"
let alder = "26"
let beskjed = "Hei, mitt navn er " ++ navn ++ " og jeg er " ++ alder ++ " år gammel."
io.println(beskjed)
```

Output:
```
Hei, mitt navn er Siri og jeg er 26 år gammel.
```

Vi kan også kombinere strenger med andre typer data, for eksempel tall:

```Gleam
let tall = 37
let beskjed = "Mitt favorittall er " ++ show(tall) ++ "!"
io.println(beskjed)
```

Output:
```
Mitt favorittall er 37!
```

## Dypere dykk
En ting å være oppmerksom på når vi kombinerer strenger er at vi må sørge for at formatet på de ulike delene stemmer overens. For eksempel må vi konvertere tall til strenger før vi kan legge dem sammen, som vist i eksemplet over med `show()` funksjonen.

Vi kan også bruke funksjoner og mønstre for å gjøre arbeidet med å kombinere strenger mer effektivt. Dette er spesielt nyttig hvis vi jobber med store mengder data og må legge sammen mange strenger.

## Se også
- [Gleam dokumentasjon for strengoperasjoner](https://gleam.run/book/stdlib.html#strings)
- [Mer om manipulering av data i Gleam](https://gleam.run/book/functional-programming.html#data-manipulation)