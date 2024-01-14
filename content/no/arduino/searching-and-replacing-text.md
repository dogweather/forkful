---
title:                "Arduino: Søking og bytting av tekst"
programming_language: "Arduino"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/arduino/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

# Hvorfor

Alle som har programmert har sannsynligvis opplevd å måtte endre tekst i koden sin, enten for å rette feil eller for å gjøre koden mer effektiv. Å kunne søke og bytte ut tekst er en grunnleggende ferdighet som kan spare deg for mye tid og arbeid når du jobber med Arduino.

# Hvordan

Å søke og bytte ut tekst i Arduino er en enkel prosess. Du kan bruke den innebygde "Find and Replace" -funksjonen i Arduino IDE, eller du kan bruke tekstredigeringsprogrammer som støtter søk og erstatt-funksjonalitet. La oss se på et eksempel der vi ønsker å endre alle forekomster av "ledPin" til "ledPin2" i koden vår.

```Arduino
int ledPin = 9;  // Definerer pin for LED-utgang
```

For å søke og erstatte tekst i Arduino IDE, går du til "Edit" -menyen og velger "Find and Replace" (Finn og erstatt). I søkefeltet skriver du inn "ledPin" og i erstatningsfeltet skriver du "ledPin2". Trykk på "Replace" (Erstatt) for å erstatte teksten eller "Replace All" (Erstatt alle) for å erstatte alle forekomster i koden.

I en tekstredigerer kan du bruke tastatursnarveier som "Ctrl + F" for å åpne søkefunksjonen og "Ctrl + H" for å åpne søke og erstatt-funksjonen. Vær oppmerksom på at det å bruke tekstredigeringsprogrammer kan være mer risikabelt ettersom de ikke har kunnskap om syntaksen til Arduino-koden din.

## Dypdykk

Når du bytter ut tekst i Arduino-koden din, er det viktig å være klar over at Arduino IDE bare tar hensyn til tegnene som er synlige i skjermbildet. Dette betyr at hvis du har koden på flere linjer, kan du ikke søke og erstatte et uttrykk som går over flere linjer. Du kan imidlertid bruke "Find" (Søk) -funksjonen til å finne forekomster av tekst og navigere til dem.

En annen ting å merke seg er at søket er casesensitivt. Dette betyr at hvis du søker etter "ledpin" vil ikke "ledPin" bli funnet og erstattet. Hvis du vil søke uten å ta hensyn til store og små bokstaver, må du huke av for "Match case" (Match case) i Arduino IDE.

# Se også

- [Introduksjon til Arduino](https://www.arduino.cc/en/Guide/Introduction)
- [Offisiell Arduino dokumentasjon](https://www.arduino.cc/reference/en/)
- [Bruk av variabler i Arduino](https://create.arduino.cc/projecthub/infusionwest/tutorial-using-variables-in-arduino-part-1-interactive-modes-627adc)
- [Feilsøking i Arduino-prosjekter](https://create.arduino.cc/projecthub/SavageCorona/arduino-troubleshooting-3a8abf)