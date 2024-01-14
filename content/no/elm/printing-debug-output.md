---
title:    "Elm: Utskrift av feilrettingsutdata"
keywords: ["Elm"]
---

{{< edit_this_page >}}

## Hvorfor

Hvis du er en Elm programmerer, har du kanskje sett debug utskrifter før. Kanskje du har brukt dem til å feilsøke en komplisert funksjon, eller bare for å se hva som skjer i bakgrunnen. Uansett grunn, er det viktig å vite hvordan man printer debug utskrifter riktig for å få mest mulig ut av dem.

## Slik gjør du det

Å printe debug utskrifter i Elm er enkelt og kan gjøres med bare et par linjer med kode. For å gjøre dette, bruker vi funksjonen `Debug.log`, som tar inn en streng og en verdi som skal printes. La oss se på et eksempel:

```Elm
import Debug

main = 
    let
        navn = "Sofie"
    in
        Debug.log "Hei, mitt navn er" navn
```

I dette eksempelet bruker vi `Debug.log` til å printe navnet vårt til konsollen. Resultatet vil se slik ut:

```
Hei, mitt navn er Sofie
```

Som du kan se, er det enkelt å printe ut informasjon ved hjelp av `Debug.log`. Dette kan også gjøres med mer komplekse verdier som lister eller tuple. Du kan til og med bruke denne funksjonen i løkker eller betingede uttrykk for å printe ut informasjon underveis i programmet ditt.

## Dykk dypere

Det er også viktig å vite at debug utskrifter bare kjøres når vi bygger appen vår med `--debug` flagget. Dette gjør at utskriftene våre kun blir vist i utviklingsmiljøet og ikke i den endelige applikasjonen. Det er også mulig å gruppere utskriftene våre og gi dem ulike nivåer av alvorlighetsgrad ved hjelp av funksjonene `Debug.log1`, `Debug.log2`, og så videre.

Det finnes også alternative måter å printe ut informasjon på i Elm, for eksempel ved å bruke `Html.programWithFlags` funksjonen og sende informasjonen vår til en side for å bli printet ut der. Men `Debug.log` funksjonen er fortsatt den mest vanlige og enkleste måten å få informasjon ut på.

## Se også

- Elm dokumentasjon: [Debug Modulen](https://package.elm-lang.org/packages/elm/core/latest/Debug)
- Artikkel: [Debugging elm with clever little prints](https://medium.com/@gdotdesign/debugging-elm-with-clever-little-prints-d3a673b5369e)
- Artikkel: [7 Tips for Debugging in Elm](https://www.developerdrive.com/7-tips-for-debugging-in-elm/)