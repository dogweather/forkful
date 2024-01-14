---
title:    "Fish Shell: Lese kommandolinje-argumenter"
keywords: ["Fish Shell"]
---

{{< edit_this_page >}}

## Hvorfor

Har du noen gang lurt på hva som skjer når du kjører et skript i terminalen? Eller hva de rare symbolene og bokstavene du legger til etter et programnavn betyr? Det er her kommandolinjeargumenter kommer inn i bildet. Les videre for å lære mer om hvordan du kan lese og bruke dem i Fish Shell.

## Hvordan

```Fish Shell``` har en innebygd funksjon som heter ```argv``` som lar deg lese alle kommandolinjeargumentene som er gitt til et program eller et skript. Det er et array som inneholder alle argumentene, inkludert programnavnet som alltid vil være det første elementet.

For eksempel, hvis du kjører følgende skript i terminalen:

```
$ ./hello.sh John Doe
```

Scriptet ville se slik ut:

```
echo Hei $argv[1] $argv[2]"
```

Output vil være:

```
Hei John Doe
```

Her bruker vi ```argv[1]``` og ```argv[2]``` for å få tilgang til de første to argumentene som ble gitt til skriptet vårt. Du kan også bruke ```argv[2:]``` for å få tilgang til alle argumentene etter det andre.

Du kan også bruke flagg eller argumenter som har en bestemt betydning i et program eller skript. For å gjøre dette kan du bruke en innebygd funksjon kalt ```has-flag``` for å sjekke om flagget er gitt. Dette er spesielt nyttig i skript som tar imot ulike argumenter og må håndtere dem forskjellig.

## Deep Dive

Kommandolinjeargumenter er nyttige når du ønsker å gi informasjon til et program eller skript uten å måtte endre selve koden. Det kan også brukes til å angi filnavn, stier, valg og mye mer.

I tillegg til å bruke ```argv``` og ```has-flag```, kan du også lese argumentene på en mer avansert måte ved å bruke en ```for```-loop. Dette lar deg håndtere hvert argument individuelt og utføre forskjellige handlinger for hvert av dem.

For eksempel kan du bruke følgende kode for å liste ut alle argumentene i terminalen:

```
for arg in $argv
  echo $arg
end
```

## Se også

- [Offisiell Fish Shell dokumentasjon](https://fishshell.com/docs/current/index.html)
- [Kommandolinjeargumenter i Bash Shell](https://www.baeldung.com/linux/bash-command-line-arguments)
- [Slik bruker du variabler i Fish Shell](https://fishshell.com/docs/latest/tutorial.html#tut_variables)