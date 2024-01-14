---
title:                "Arduino: Työskentely JSON:n kanssa."
simple_title:         "Työskentely JSON:n kanssa."
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/arduino/working-with-json.md"
---

{{< edit_this_page >}}

## Miksi JSON on tärkeä Arduino-ohjelmoinnissa?

JSON (JavaScript Object Notation) on kevyt ja helppokäyttöinen tapa tallentaa ja lähettää dataa ohjelmoinnissa. Se on erityisen hyödyllinen Arduino-mikrokontrollerin ohjelmoinnissa, koska se voi käsitellä erilaisia datatyyppejä kuten tekstiä, numeroita ja listoja. JSON mahdollistaa myös järjestelmien välisen kommunikoinnin, mikä on tärkeää monissa IoT-sovelluksissa.

## Kuinka käyttää JSONia Arduino-ohjelmoinnissa?

Käyttämällä ArduinoJSON-kirjastoa, voit helposti lukea ja kirjoittaa JSON-tietoja Arduino-koodissa. Alla on esimerkkejä käyttäen Arduino UNO -levyä ja LED-valoa:

```arduino
#include <ArduinoJson.h>

// Luodaan LED-pinny (13) muuttuja nimeltä "ledPin"
int ledPin = 13;

// Alustetaan JSON-muuttuja nimeltä "data"
StaticJsonDocument<200> data;

void setup() {
  // Asetetaan ledPin pinniksi OUTPUT
  pinMode(ledPin, OUTPUT);

  // Tallennetaan JSON-muuttujaan data erilaisia tietoja
  data["nimi"] = "Arduino";
  data["luokka"] = "Mikrokontrolleri";
  data["käyttöjärjestelmä"] = "C++, Arduino IDE";

  // Convertoidaan data JSON-muotoon ja tulostetaan sarjaporttiin
  serializeJson(data, Serial);
}

void loop() {
  // Lukee JSON-taulukon arvon nimeltä "tila" ja tallentaa sen muuttujaan "valo"
  int valo = data["tila"];

  // Jos valon tila on "päällä", niin sytytetään LED
  if (valo == "päällä") {
    digitalWrite(ledPin, HIGH);
  }
  // Muussa tapauksessa sammutetaan LED
  else {
    digitalWrite(ledPin, LOW);
  }
}
```

Tulostus sarjaporttiin näyttäisi tältä:

```json
{"nimi":"Arduino","luokka":"Mikrokontrolleri","käyttöjärjestelmä":"C++, Arduino IDE"}
```

## Syvemmälle JSON-ohjelmointiin

JSON-rakenteet koostuvat avain-arvo pareista, jotka on eroteltu kaksoispisteellä. Avaimet ovat aina merkkijonoja, kun taas arvot voivat olla tekstiä, numeroita, listoja tai jopa toisia JSON-rakenteita.

JSON-taulukot ovat listoja, jotka on ympäröity hakasulkeilla ja erillisillä arvoilla pilkulla. Tämä mahdollistaa usean arvon tallentamisen yhteen avain-arvo pariin.

ArduinoJSON-kirjasto tarjoaa useita toimintoja JSON-muuttujien käsittelyyn, kuten lisääminen, poisto ja tarkistaminen. Lisätietoja näistä toiminnoista löytyy kirjaston dokumentaatiosta.

## Katso myös

- [ArduinoJSON-kirjasto](https://arduinojson.org/)
- [JSON-opas](https://www.json.org/json-fi.html)
- [Esimerkkejä JSON-ohjelmoinnista Arduino-ympäristössä](https://www.arduino.cc/playground/Code/Json)
- [IoT-sovellukset Arduino-mikrokontrollerilla](https://www.arduino.cc/)