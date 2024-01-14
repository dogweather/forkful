---
title:    "Fish Shell: Sammenligne to datoer"
keywords: ["Fish Shell"]
---

{{< edit_this_page >}}

## Hvorfor

Å sammenligne to datoer kan være viktig for å organisere og filtrere data. I Fish Shell er det enkelt å sammenligne datoer ved hjelp av innebygde funksjoner og enkle syntaks.

## Hvordan

For å sammenligne to datoer i Fish Shell, bruker man `date` kommandoen og `<=` eller `>=` operatorer. Her er et eksempel på å sammenligne to datoer i formatet DD/MM/YYYY:

```
Fish Shell $ date -s '01/01/2020'
Fish Shell $ if date -s '02/02/2020' <= date -s '01/01/2020'
    echo "Dato 2 er før eller lik Dato 1"
else
    echo "Dato 2 er etter Dato 1"
end
```

Dette vil gi følgende utgang:

```
Dato 2 er etter Dato 1
```

Her brukte vi `<=` operator for å sjekke om Dato 2 er før eller lik Dato 1.

Man kan også bruke `>=` operator på samme måte for å sjekke om Dato 2 er etter eller lik Dato 1.

## En dypdykk

Det er viktig å merke seg at sammenligningsmetoden vil variere avhengig av formatet på datoene. For eksempel, hvis datoene er i formatet YYYYMMDD, må man bruke en annen metode for å sammenligne dem.

En annen ting å huske på er at Fish Shell også har innebygde funksjoner for å konvertere datoer til enklere formater. For eksempel, kan man bruke `date -s` kommandoen for å konvertere datoer til enklere format som DD/MM/YYYY.

Så, når man sammenligner to datoer, er det viktig å ta hensyn til formatet og eventuelt konvertere det så det passer med sammenligningsmetoden som brukes.

## Se også

- [Fish Shell dokumentasjon for date kommandoen](https://fishshell.com/docs/current/cmds/date.html)
- [Guide for å sammenligne datoer i Bash Shell](https://stackoverflow.com/questions/4944340/compare-two-dates-with-linux-shell-script)
- [Innbygde funksjoner for å jobbe med datoer i Fish Shell](https://fishshell.com/docs/current/tutorial.html#date)