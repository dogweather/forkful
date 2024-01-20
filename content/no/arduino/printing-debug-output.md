---
title:                "Utskrift av feilsøkingsresultat"
html_title:           "Arduino: Utskrift av feilsøkingsresultat"
simple_title:         "Utskrift av feilsøkingsresultat"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/arduino/printing-debug-output.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Trykking av feilsøkingsutdata er det når programmerere logger data for å spore variabler og systemtilstand under kjøringen av et program. Dette gjør at de kan identifisere og fikse feilene raskere og mer effektivt.

## Hvordan:
Her er noen nyttige eksempler på hvordan du kan prøve ut feilsøkingsutskrift i Arduino.

```Arduino
void setup() {
  Serial.begin(9600); //starter serial kommunikasjon  
}
void loop() {
  int sensorValue = analogRead(A0); //leser sensor verdien fra A0 pin
  Serial.println(sensorValue); //printer sensorverdien for feilsøk
  delay(1000); //venter i 1 sekund
}
```

Når du laster opp denne koden, vil Arduino lese sensorverdien hvert sekund og skrive den ut i den serielle monitoren. Dette hjelper oss med å holde oversikt over sensordataene.

## Dyp Dykk
Historisk ble feilsøkingsutskrift brukt fordi verktøyene for interaktiv feilsøking var primitive eller ikke-eksisterende. Selv om vi nå har mer sofistikerte feilsøkingsverktøy, er feilsøkingsutskrift fortsatt et nyttig verktøy på grunn av dets enkelhet og brede bruksområder.

Det finnes alternativer til feilsøkingsutskrift, som f.eks interaktive feilsøkere, som gir mer detaljert informasjon og mer avansert kontroll over kjøringen. Men de kan være mer kompliserte og overkill for mange situasjoner.

Fra implementeringsperspektivet er `Serial.print` og `Serial.println` funksjoner i Arduino brukt for å sende data (tekst, variabler) fra Arduino til datamaskinen over USB-tilkoblingen. Disse funksjonene er en del av Serial Library.

## Se Også
1. [Arduino Reference - Serial](https://www.arduino.cc/reference/en/language/functions/communication/serial/)
2. [Arduino debug techniques](https://www.baldengineer.com/arduino-debug-techniques.html)
3. [Understanding the Serial.print() Function in Arduino](https://www.makerguides.com/arduino-serial/)