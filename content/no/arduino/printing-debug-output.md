---
title:                "Utskrift av feilsøkingsutdata"
html_title:           "Arduino: Utskrift av feilsøkingsutdata"
simple_title:         "Utskrift av feilsøkingsutdata"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/arduino/printing-debug-output.md"
---

{{< edit_this_page >}}

## Hvorfor

Å printe debug output er en viktig del av å feilsøke og forbedre koden din. Det lar deg se nøyaktig hva som skjer under kjøring, slik at du kan identifisere og løse eventuelle problemer.

## Hvordan

For å printe debug output på din Arduino, kan du bruke funksjonen `Serial.print()`. Denne funksjonen tar inn en parameter, for eksempel en variabel eller en tekststreng, og sender den til seriell porten på Arduinoen din. Deretter kan du åpne seriell monitor i Arduino IDE for å lese og analysere outputen.

```Arduino
int sensorValue = 0;
void setup() {
  Serial.begin(9600); //åpner seriell port
}
void loop() {
  sensorValue = analogRead(A0); //leser verdi fra analog pin A0
  Serial.print("Sensoren leser: "); //printer teksten før variabelen
  Serial.println(sensorValue); //printer variabelen og legger til linjeskift
  delay(1000); //venter i 1 sekund før kode kjøres på nytt
}
```

Outputen fra dette eksempelet vil se ut som dette:

```
Sensoren leser: 378
```

Dette lar deg se den aktuelle verdien som sensoren leser, og du kan gjøre endringer og se om verdien endres som forventet.

## Deep Dive

Det er også mulig å formatere outputen for å gjøre den mer lesbar. For eksempel kan du bruke `Serial.print()` flere ganger for å legge til flere variabler eller tekstrenger i samme linje. Du kan også bruke `Serial.println()` for å legge til linjeskift etter hver linje med output.

```Arduino
int temp = 25;
float humidity = 50.3;
void setup() {
  Serial.begin(9600);
}
void loop() {
  Serial.print("Temperatur: ");
  Serial.print(temp);
  Serial.print(" °C, Fuktighet: ");
  Serial.print(humidity);
  Serial.println("%");
  delay(1000);
}
```

Outputen fra dette eksempelet vil se ut som dette:

```
Temperatur: 25 °C, Fuktighet: 50.3%
```

Dette gjør outputen mer forståelig og enklere å analysere.

## Se Også

- [Arduino Serial](https://www.arduino.cc/reference/en/language/functions/communication/serial/)
- [How to Use Serial Print in Arduino?](https://maker.pro/arduino/tutorial/how-to-use-serial-print-in-arduino)
- [Debugging Arduino Sketches with Serial.println()](https://learn.sparkfun.com/tutorials/debugging-arduino-sketches-with-serial-println)