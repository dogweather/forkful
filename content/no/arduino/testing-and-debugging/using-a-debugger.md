---
title:                "Å bruke en feilsøker"
aliases:
- no/arduino/using-a-debugger.md
date:                  2024-01-26T03:47:16.467523-07:00
model:                 gpt-4-0125-preview
simple_title:         "Å bruke en feilsøker"

tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/arduino/using-a-debugger.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?

En feilsøker er et verktøy som hjelper deg med å klemme ut feil i koden din ved å la deg pause, utforske, og finne ut hva som egentlig foregår under panseret. Programmerere bruker feilsøkere for å gå gjennom koden sin, inspisere variabler, og forstå hvor ting kanskje går galt.

## Hvordan:

Med Arduino IDE kan du bruke serieutskrifter til å feilsøke, men det er litt som å bruke en lommelykt for å utforske en hule. For ekte feilsøking, kan du ønske å oppgradere spillet ditt med noe som Atmel-ICE feilsøkeren som integreres med Arduino-miljøet. Her er en smak av pseudo-feilsøking ved hjelp av Serial:

```Arduino
void setup() {
  Serial.begin(9600);
}
void loop() {
  int sensorValue = analogRead(A0);
  Serial.print("Sensorverdi: ");
  Serial.println(sensorValue);
  // Forestill deg at du forventer 512 her, men får 0.
  // På tide å inspisere sensorforbindelsen
  delay(1000); // Vent i et sekund før du leser igjen
}
```
Kjør dette med Seriell-skjermen åpen, og du vil se hva sensoren din spyttet ut i sanntid.

## Dypdykk

Før feilsøkere, var det trykkeutskriftens verden – du kunne bare gjette hva som skjedde ved å skrive ut alt. Debugging med utskrifter er fremdeles vanlig, spesielt i enklere miljøer eller på begrenset maskinvare som Arduino.

Alternativer til inkretsemulatorer som Atmel-ICE inkluderer programvarefeilsøkingsverktøy som `avr-gdb`. Du kan parre den med `avarice` for å skape en bro mellom GDB og maskinvaren din, noe som er super hendig for mer avansert feilsøking rett på chipen.

Ved å bruke en feilsøker, kan du sette brytepunkter for å stoppe utførelsen på visse punkter. Du kan gå gjennom koden din linje for linje, inspisere minne, registre og variabler. Dette lar deg peke ut problemer i stedet for å skyte i mørket. Når du implementerer en feilsøker, sørg for at miljøet ditt er riktig oppsatt - feilpassede versjoner eller dårlig konfigurerte verktøy kan føre til frustrasjon.

## Se også

Klar til å dykke dypere? Utforsk disse:
- Arduino-feilsøkingsguiden på [Arduino Debugging](https://www.arduino.cc/en/Guide/Environment#toc7)
- AVR Libc referansemanualen for å sette opp avr-gdb: [AVR Libc hjemmeside](http://www.nongnu.org/avr-libc/)
