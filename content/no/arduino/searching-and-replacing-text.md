---
title:                "Søking og utskifting av tekst"
html_title:           "Arduino: Søking og utskifting av tekst"
simple_title:         "Søking og utskifting av tekst"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/arduino/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Hvorfor
Hvorfor skulle du trenge å søke og erstatte tekst i din Arduino kode? Vel, det er faktisk et nyttig verktøy hvis du ønsker å gjøre masseendringer i koden din. Det kan også være nyttig for å endre f.eks. pin-numre eller verdier i variabler på en enkel måte.

## Hvordan
For å søke og erstatte tekst i Arduino, kan du bruke den innebygde funksjonen "Find and Replace". Dette gjøres ved å først åpne Arduino-programvaren og deretter trykke på "Ctrl + F" (for Windows) eller "Cmd + F" (for Mac). Dette vil åpne et søkefelt der du kan skrive inn teksten du ønsker å søke etter i koden din.

```Arduino
#define LED_PIN 5
void setup(){
  pinMode(LED_PIN, OUTPUT);
}
void loop(){
  digitalWrite(LED_PIN, HIGH);
  delay(1000);
  digitalWrite(LED_PIN, LOW);
  delay(1000);
}
```

For eksempel, hvis vi ønsker å endre LED-pinnen fra 5 til 6, kan vi søke etter "LED_PIN 5" og erstatte det med "LED_PIN 6". Det vil se slik ut:

```Arduino
#define LED_PIN 6
void setup(){
  pinMode(LED_PIN, OUTPUT);
}
void loop(){
  digitalWrite(LED_PIN, HIGH);
  delay(1000);
  digitalWrite(LED_PIN, LOW);
  delay(1000);
}
```

Vi kan også bruke "Find and Replace" til å endre verdien i en variabel. La oss si at vi ønsker å endre delay-verdien i koden vår fra 1000 millisekunder til 500 millisekunder. Vi søker da etter "delay(1000)" og erstatter det med "delay(500)". Koden vil da se slik ut:

```Arduino
#define LED_PIN 6
void setup(){
  pinMode(LED_PIN, OUTPUT);
}
void loop(){
  digitalWrite(LED_PIN, HIGH);
  delay(500);
  digitalWrite(LED_PIN, LOW);
  delay(500);
}
```

## Dypdykk
Det er noen ting å være oppmerksom på når du bruker "Find and Replace" i Arduino-koden din. Det er viktig å sørge for at du kun erstatter det du ønsker å endre og ikke noe annet i koden. Det kan være lurt å teste koden din etterpå for å være sikker på at alt fortsatt fungerer som det skal.

Du bør også være nøye med kassebruk (store og små bokstaver) når du søker og erstatter tekst. Arduino er følsom for kassebruk og vil ikke gjenkjenne ord som er skrevet på feil måte.

## Se også
- [Arduino offisiell nettside](https://www.arduino.cc/)
- [Offisiell Arduino Forum](https://forum.arduino.cc/)
- [Arduino dokumentasjon](https://www.arduino.cc/reference/en/)