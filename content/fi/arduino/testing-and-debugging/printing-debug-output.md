---
title:                "Virheenjäljitystulosteiden tulostaminen"
aliases:
- /fi/arduino/printing-debug-output/
date:                  2024-01-20T17:51:49.616511-07:00
model:                 gpt-4-1106-preview
simple_title:         "Virheenjäljitystulosteiden tulostaminen"

tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/arduino/printing-debug-output.md"
---

{{< edit_this_page >}}

## What & Why? - Mikä ja Miksi?
Debug-tulostus auttaa näkemään mitä ohjelmassasi tapahtuu. Sitä käytetään virheiden etsintään ja ohjelman toiminnan ymmärtämiseen.

## How to - Kuinka tehdä:
```Arduino
void setup() {
  Serial.begin(9600); // Käynnistetään sarjayhteys
}

void loop() {
  Serial.println("Tämä on debug-viesti."); // Tulostetaan debug-viesti
  delay(1000); // Viivytetään sekunti
}
```

Esimerkkitulostus:
```
Tämä on debug-viesti.
Tämä on debug-viesti.
...
```

## Deep Dive - Syväsukellus:
Debug-tulostuksen historia juontaa juurensa ohjelmoinnin alkuhämäriin, jolloin se oli yksi harvoista tavoista seurata ohjelman toimintaa reaaliaikaisesti. Vaihtoehtoina debug-tulostukselle ovat muun muassa LEDien vilkuttaminen tai reaaliaikaiset debuggerit. Arduino käyttää sarjaliikennettä (Serial) debug-tulostukseen, mikä on yksinkertaista mutta tehokasta – data kulkee USB:n kautta tietokoneelle, missä voimme lukea sen sarjaportin monitorista.

## See Also - Katso Myös:
- Arduino'n virallinen dokumentaatio Serial-käskystä: https://www.arduino.cc/reference/en/language/functions/communication/serial/
- Arduino debugging techniques: https://www.arduino.cc/en/Tutorial/BuiltInExamples/SerialEvent
- Beginners' guide to Arduino programming: https://www.arduino.cc/en/Guide/HomePage
