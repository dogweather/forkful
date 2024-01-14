---
title:    "Arduino: Søking og bytting av tekst"
keywords: ["Arduino"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/no/arduino/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Hvorfor

Hvis du noen gang har programmert med Arduino, så har du sannsynligvis kommet over en uforutsett feil eller et problem som må løses. Kanskje du har skrevet den samme koden flere ganger, men med små endringer? Dette er hvor "søk og erstatt" funksjonen kommer inn i bildet - den kan hjelpe deg med å effektivt finne og bytte ut tekst i koden din.

## Hvordan

La oss si at du vil endre alle forekomster av "LED" til "LYSDIODER" i koden din. Dette kan gjøres enkelt ved å bruke "Søk og erstatt" funksjonen i Arduino IDE. Følg disse trinnene:

1. Åpne Arduino IDE og åpne koden du ønsker å søke og erstatte tekst i.
2. Trykk på "Ctrl + F" eller gå til "Edit" menyen og velg "Søk og erstatt".
3. Skriv inn "LED" i "Søk:" feltet og "LYSDIODER" i "Erstatt med:" feltet.
4. Klikk på "Erstatt" eller "Erstatt alle" for å bytte ut alle forekomster av "LED" med "LYSDIODER".

```
Arduino kode:
int ledPin = 13;
digitalWrite(ledPin, HIGH);
```
```
Arduino kode etter "Søk og erstatt":
int lysdiodePin = 13;
digitalWrite(lysdiodePin, HIGH);
```

Dette er en enkel måte å effektivt endre tekst i koden din uten å måtte gå gjennom hver linje manuelt.

## Dykk dypere

"Søk og erstatt" funksjonen i Arduino IDE gir også flere muligheter for mer avansert tekstbehandling. Du kan for eksempel bruke regulære uttrykk for å finne og endre tekst basert på bestemte mønstre, eller du kan raskt bytte ut tekst i flere filer samtidig.

En annen måte å finne og erstatte tekst på er å bruke biblioteker, som "String.replace()" funksjonen i String biblioteket. Dette kan være nyttig hvis du jobber med tekst lagret som en variabel i koden din.

## Se også

- [ Arduino IDE brukerdokumentasjon ](https://www.arduino.cc/en/Guide/Environment)
- [ Arduino "Søk og erstatt" dokumentasjon ](https://www.arduino.cc/en/Reference/EditReplace)