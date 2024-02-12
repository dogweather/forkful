---
title:                "Komennoriviparametrien lukeminen"
aliases:
- /fi/arduino/reading-command-line-arguments.md
date:                  2024-01-20T17:55:53.843687-07:00
model:                 gpt-4-1106-preview
simple_title:         "Komennoriviparametrien lukeminen"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/arduino/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## What & Why? (Mitä ja Miksi?)
Komennon riviargumentit mahdollistavat erilaisten arvojen syöttämisen ohjelmallesi sen käynnistyessä. Tämä auttaa tekemään ohjelmasta joustavamman, sillä voit muokata ohjelman toimintaa ilman koodin muuttamista.

## How to: (Kuinka tehdään:)
Arduino-ympäristössä ei käytetä perinteisiä komentoriviargumentteja kuten tietokoneen ohjelmissa, koska Arduino koodi ladataan mikrokontrollerille, ja se ei käynnisty komentoriviltä. Voit kuitenkin lukea syötteitä sarjaportin kautta, mikä toimii samankaltaisena ratkaisuna.

```
Arduino
void setup() {
  Serial.begin(9600); // Käynnistä sarjaportti nopeudella 9600 bittiä sekunnissa
}

void loop() {
  if (Serial.available() > 0) {
    String inputData = Serial.readStringUntil('\n'); // Lue rivi
    Serial.println("Syötteesi oli: " + inputData);
  }
}
```

Lähetä dataa Arduinolle Serial Monitorin kautta. Näet tulosteen:
```
Syötteesi oli: sinun_syöttämä_arvo
```

## Deep Dive (Syväsukellus):
Arduinon maailmassa komentoriviargumenttien puute johtuu laitteiston rajoitteista – Arduinot eivät käynnisty perinteisessä mielessä eikä niissä ole käyttöjärjestelmää, joka hallinnoisi argumentteja. Vaihtoehtoisesti syötteiden käsittely tapahtuu sarjaportin kautta. Historiallisesti Arduinon ympäristö on keskittynyt helppoon käyttöönottoon ja yksinkertaiseen I/O-toimintaan. Siksi sarjaportti on jäänyt pääasialliseksi tavaksi syötteen vastaanottamiselle ja prosessoinnille. 

Sarjaportti on kuitenkin tehokas työkalu datan siirtämiseen Arduinon ja tietokoneen tai muiden laitteiden välillä. Se mahdollistaa tietynlaisten argumenttien "simuloinnin" lähettämällä syötteitä ohjelman suorituksen aikana.

## See Also (Katso Myös):
- Arduinon virallinen sarjaportti ohjeistus: https://www.arduino.cc/reference/en/language/functions/communication/serial/
- Esimerkkejä sarjaportin käytöstä datan siirrossa: https://www.arduino.cc/en/Tutorial/BuiltInExamples/SerialEvent
- Tietoa Arduinon I/O-toiminnoista: https://www.arduino.cc/en/Reference/BoardGPIO
