---
title:    "Arduino: Å lese kommandolinjeargumenter"
keywords: ["Arduino"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/no/arduino/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Hvorfor

Hvis du allerede er en erfaren Arduino-programmerer, så har du sannsynligvis hørt om å lese kommandolinjeargumenter. Dette kan virke som en ekstra trinn i kodingen din, men det kan være en nyttig måte å forbedre programmene dine på. I denne bloggposten skal vi utforske hvorfor du bør lære å lese kommandolinjeargumenter og hvordan du kan gjøre det på en enkel måte.

## Hvordan

Å lese kommandolinjeargumenter i Arduino-programmering kan hjelpe deg med å gjøre programmene dine mer fleksible og tilpassbare. La oss se på et enkelt eksempel på hvordan du kan lese en kommandolinjeargument og bruke den i koden din:

```Arduino
int num = 0; //oppretter en variabel for å lagre argumentet

void setup() {
  Serial.begin(9600); //starter seriell kommunikasjon
  while (!Serial); //venter på at Serial skal være tilkoblet
  if (Serial.available()) { //sjekker om det finnes et argument
    num = Serial.parseInt(); //leser argumentet og lagrer det i variabelen
  }
}

void loop() {
  //bruker variabelen i koden
  Serial.println("Det oppgitte nummeret er: " + String(num));
}
```

Her bruker vi funksjonen `Serial.parseInt()` til å lese et heltall fra kommandolinjen og lagrer det i variabelen `num`. Dette gjør det mulig for brukeren å endre verdien til `num` uten å måtte endre koden direkte. Du kan deretter bruke denne variabelen som du ønsker i koden din.

## Dypdykk

Nå som du har sett et enkelt eksempel på hvordan du kan lese kommandolinjeargumenter i Arduino, la oss dykke litt dypere og se på noen viktige ting du bør huske på.

- Du kan lese kommandolinjeargumenter ved hjelp av både `Serial.read()` og `Serial.parseInt()` funksjonene, avhengig av hva slags data du forventer å motta.
- Husk å sjekke om det faktisk finnes et argument tilgjengelig før du prøver å lese det ved hjelp av `Serial.available()` funksjonen.
- Du kan også lese mer enn ett argument fra kommandolinjen ved å bruke en `for`-løkke og `Serial.read()` eller `Serial.parseInt()` i kombinasjon med `Serial.available()`.

Å lese kommandolinjeargumenter kan være nyttig for å lage mer dynamiske og tilpassbare programmer. Det er også en viktig del av å lære å kommunisere med eksterne enheter og datakilder.

## Se også

- [Arduino Serial kommunikasjon](https://www.arduino.cc/reference/en/language/functions/communication/serial/)
- [Hvordan kommunisere med Arduino over serielt grensesnitt](https://www.arduino.cc/en/Guide/ArduinoSerial)
- [Lære å programmere i Arduino](https://www.arduino.cc/en/Tutorial/HomePage)