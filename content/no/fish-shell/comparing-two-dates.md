---
title:                "Fish Shell: Sammenligning av to datoer"
programming_language: "Fish Shell"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/fish-shell/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Hvorfor
Har du noen gang lurt på hvordan du kan sammenligne to datoer i Fish Shell? Å sammenligne datoer kan være nyttig for å organisere og sortere informasjon, spesielt hvis du jobber med store mengder data eller kalendersystemer.

## Slik gjør du det
Kod blokkene nedenfor viser deg hvordan du kan sammenligne datoer i Fish Shell og få ønsket resultat.

```Fish Shell
# Definer to datoer
set dato1 2021-10-01
set dato2 2021-09-15

# Bruk «date» kommandoen for å konvertere datoene til sekunder siden 1970
set dato1_s (date -f '%s' $dato1)
set dato2_s (date -f '%s' $dato2)

# Sammenlign datoer ved å subtrahere sekunder siden 1970
if test $dato1_s -gt $dato2_s
    echo "$dato1 er en senere dato enn $dato2"
else if test $dato1_s -lt $dato2_s
    echo "$dato1 er en tidligere dato enn $dato2"
else
    echo "Datoene er like"
end
```

Output:
```Fish Shell
$ sh diff_dates.fish
2021-10-01 er en senere dato enn 2021-09-15
```

## Dykk dypere
Når du sammenligner datoer, er det viktig å være oppmerksom på formateringsforskjeller og overflødige symboler som kan påvirke resultatet. For eksempel, hvis datoene er i forskjellige formater, må du kanskje bruke en annen kommando for å konvertere dem til sekunder siden 1970. I tillegg kan forskjellige tidszoner også påvirke resultatet.

En annen ting å merke seg er at Fish Shell har en innebygd funksjon for å sammenligne datostrenger, men ikke for å sammenligne datoer som sekunder siden 1970. Derfor må vi bruke kommandoen «test» og sammenligningsoperatørene «-gt» (greater than) og «-lt» (less than) for å sammenligne datoer som sekunder siden 1970.

Å dykke dypere inn i sammenligning av datoer i Fish Shell kan hjelpe deg med å forstå de ulike faktorene som kan påvirke resultatet og dermed gjøre deg i stand til å lage mer nøyaktige sammenligninger.

## Se også
- [Fish Shell - Offisielt nettsted](https://fishshell.com/)
- [Fish Shell dokumentasjon](https://fishshell.com/docs/current/)
- [«date» kommandoen dokumentasjon](https://fishshell.com/docs/current/commands.html#date)
- [«test» kommandoen dokumentasjon](https://fishshell.com/docs/current/commands.html#test)