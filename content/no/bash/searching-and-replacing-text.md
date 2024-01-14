---
title:    "Bash: Reply withSøking og utskifting av tekst"
keywords: ["Bash"]
---

{{< edit_this_page >}}

## Hvorfor

Å søke og erstatte tekst er en vanlig oppgave for programmerere og datakyndige. Det kan være nødvendig å endre navn på variabler, rette opp skrivefeil eller gjøre store endringer i en tekstfil. Ved å automatisere denne prosessen med Bash-programmering, kan du spare tid og unngå manuelle feil. 

## Hvordan

Det finnes flere måter å søke og erstatte tekst på i Bash. En av de mest effektive måtene er å bruke `sed`-kommandoen, som står for "stream editor". Denne kommandoen lar deg gjøre endringer i en tekstfil uten å endre den opprinnelige filen.

La oss si at du ønsker å endre alle forekomster av ordet "hund" til "katt" i en tekstfil kalt "dyr.txt". Dette kan gjøres ved å bruke følgende kommando i Bash:

```Bash
sed -i 's/hund/katt/g' dyr.txt
```

La oss gå litt dypere inn i denne kommandoen. Det første vi ser er flagget `-i`, som står for "in-place". Dette gjør at endringene vi gjør vil bli lagret i den opprinnelige filen i stedet for å bli skrevet ut i terminalen.

Deretter ser vi `'s/hund/katt/g'`, som er selve søke- og erstatte-mønsteret. Her står "s" for "substitute", altså "erstatt". Så kommer ordet du ønsker å erstatte, etterfulgt av det nye ordet. Til slutt kommer "g" som står for "global", som betyr at alle forekomster av ordet blir erstattet, ikke bare den første.

Etter at kommandoen er utført vil dyr.txt-filen nå inneholde "katt" i stedet for "hund" overalt.

## Dypdykk

Det finnes mange andre måter å bruke `sed`-kommandoen på for å søke og erstatte tekst i Bash. Du kan for eksempel bruke regulære uttrykk for å søke etter et mønster istedenfor et konkret ord. Du kan også kombinere flere kommandoer og bruke variabler for mer avansert tekstmanipulasjon.

Det er også verdt å nevne at `sed`-kommandoen ikke bare kan brukes i Bash, men også i andre programmeringsspråk og tekstbehandlingsprogrammer.

## Se også

Hvis du ønsker å lære mer om `sed`-kommandoen og andre nyttige Bash-kommandoer, kan du sjekke ut disse ressursene:

- [The Bash Academy](https://www.bash.academy/)
- [Bash Guide for Beginners](http://tldp.org/LDP/Bash-Beginners-Guide/html/)
- [Linux Documentation Project](http://www.tldp.org/)

Lykke til med å søke og erstatte tekst i Bash!