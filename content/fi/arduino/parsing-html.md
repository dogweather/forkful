---
title:                "HTML:n jäsentäminen"
date:                  2024-01-20T15:30:16.209141-07:00
html_title:           "Bash: HTML:n jäsentäminen"
simple_title:         "HTML:n jäsentäminen"

category:             "Arduino"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/arduino/parsing-html.md"
---

{{< edit_this_page >}}

## What & Why? (Mitä & Miksi?)
HTML:n jäsentäminen tarkoittaa HTML-koodin rakenteen lukemista ja sen sisällön erottelua. Ohjelmoijat tekevät tätä tiedon kaivamiseksi tai ihmisten käyttämiä verkkosivuja ohjelmallisesti käsiteltäväksi.

## How to: (Kuinka tehdä:)
Arduino ei luontaisesti tue HTML:n jäsentämistä, joten käytämme kolmannen osapuolen kirjastoa. Tässä esimerkki käyttäen `ESP8266`-sarjaa ja kirjastoa `ESP8266HTTPClient.h`:

```Arduino
#include <ESP8266WiFi.h>
#include <ESP8266HTTPClient.h>

const char* ssid = "yourSSID";
const char* password = "yourPassword";

void setup() {
  Serial.begin(115200);
  WiFi.begin(ssid, password);

  while (WiFi.status() != WL_CONNECTED) {
    delay(1000);
    Serial.println("Connecting to WiFi...");
  }

  HTTPClient http;
  http.begin("http://example.com"); // Vaihda osoitteeseen, josta haluat jäsentää HTML:n
  int httpCode = http.GET();

  if (httpCode > 0) {
    String payload = http.getString();
    // Tässä kohdassa voit jäsentää payload-muuttujan HTML:n
  } else {
    Serial.println("Error in HTTP request");
  }
  http.end();
}

void loop() {
  // Tässä ei tarvita toistoa jäsentämiseen liittyen
}
```

Huomaa, että itse jäsentäminen vaatii vielä koodia. Tämä esimerkki näyttää vain HTML-sisällön noutamisen.

## Deep Dive (Syväsukellus):
HTML:n jäsentäminen Arduinolla on harvinaista, mutta mahdollista. Aikaisemmin jäsentäminen tehtiin pääasiassa palvelimilla, mutta IoT:n myötä tarve on siirtynyt laitteisiin. Jäsennysmenetelmiä on erilaisia, kuten säännöllisten lausekkeiden käyttö tai DOM-puun rakentaminen. Tarkkuus ja tehokkuus riippuvat käytetystä metodista. Arduinolle suunniteltu jäsentäminen on yksinkertaistettua laitekapasiteetin takia. Vaihtoehtoisesti voit käyttää mikrokontrolleria välittäjänä ja siirtää raskaamman jäsentämisen palvelimelle.

## See Also (Katso myös):
- Arduino String -luokan dokumentaatio: https://www.arduino.cc/reference/en/language/variables/data-types/string/
- ESP8266WiFi-kirjaston GitHub: https://github.com/esp8266/Arduino/tree/master/libraries/ESP8266WiFi
- RegExp (säännölliset lausekkeet) - Arduino-opas: https://www.arduino.cc/reference/en/libraries/regex/
- Yleinen HTML:n jäsentämisen opas (ei erityinen Arduino): https://developer.mozilla.org/en-US/docs/Web/HTML/Parsing
