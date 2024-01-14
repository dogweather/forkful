---
title:    "Fish Shell: Fjerning av tegn som samsvarer med et mønster"
keywords: ["Fish Shell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/no/fish-shell/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Hvorfor

Hvorfor ville du engasjere deg i å slette tegn som matcher et mønster? Vel, det kan være flere forskjellige grunner til dette. Kanskje du har en tekststreng som inneholder et uønsket tegn og du ønsker å fjerne det for å forbedre lesbarheten. Eller kanskje du jobber med regelmessige uttrykk og trenger å fjerne bestemte tegn for å matche et mønster. Uansett hva grunnen din måtte være, kan Fish Shell gjøre denne prosessen enkel og effektiv.

## Hvordan

For å slette tegn som matcher et mønster i Fish Shell, kan du bruke kommandoen `string replace`. Denne kommandoen sletter alle forekomster av et bestemt tegn eller tegnrekke i en tekststreng.

For å bruke denne kommandoen, må du først skrive `string replace` etterfulgt av mønsteret du vil matche og hva du vil erstatte det med, separert med et mellomrom. For eksempel, hvis du ønsker å slette alle mellomrom i en tekststreng og erstatte dem med ingenting, kan du skrive `string replace " " ""` etterfulgt av tekststrengen du ønsker å endre. Her er et eksempel på hvordan dette kan se ut i Fish Shell:

```Fish Shell
string replace " " "" Dette er en tekststreng
```

Output:
```Fish Shell
Detteerentekststreng
```

Her er et annet eksempel, hvor vi sletter alle tall fra en tekststreng:

```Fish Shell
string replace [0-9] "" Dette er en tekst med tall 123
```

Output:
```Fish Shell
Dette er en tekst med tall
```

## Dypdykk

Nå som du har lært hvordan du kan slette tegn som matcher et mønster i Fish Shell, la oss ta en dypere titt på hva som faktisk skjer når du bruker `string replace` kommandoen. Når du sletter forekomster av et mønster i en tekststreng, blir disse tegnene erstattet med ingenting, slik at teksten din blir kortere.

Det er også verdt å merke seg at `string replace` kommandoen støtter bruk av regelmessige uttrykk. Dette betyr at du kan bruke mer komplekse mønstre for å matche og slette tegn i tekststrengen din.

## Se også

- [Fish Shell dokumentasjon](https://fishshell.com/docs/current/index.html)
- [Regulære uttrykk i Fish Shell](https://fishshell.com/docs/current/tutorial.html#tut_strings)
- [Bruke `string replace` kommandoen](https://fishshell.com/docs/current/commands.html#string-replace)