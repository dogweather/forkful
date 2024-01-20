---
title:                "Tilapäisen tiedoston luominen"
html_title:           "Arduino: Tilapäisen tiedoston luominen"
simple_title:         "Tilapäisen tiedoston luominen"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/arduino/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

# Mitä & Miksi?
Väliaikaisten tiedostojen luonti merkitsee väliaikaismuotoisen tiedostorakenteen, kuten datan, luomista käyttöjärjestelmässä. Tämä tehdään tyypillisesti silloin, kun tarvitset tilapäisen varaston dataa varten, jotta voidaan säästää muistia tai välttyä lopullisten tiedostojen turmeltumiselta.

# Kuinka se tehdään:
Seuraava koodi luo väliaikaisen tiedoston SD-kortille:

```Arduino
#include <SD.h>
 
File tempFile;

void setup() {
  Serial.begin(9600);
  if (!SD.begin(4)) { 
    Serial.println("Card Failure");
    return;
  }
  tempFile = SD.open("temp.txt", FILE_WRITE);
  tempFile.println("Testing 1, 2, 3");
  tempFile.close();
  Serial.println("File created");
}

void loop() {
  // Put your main code here.
}
```

Tämä luonnos luo  "temp.txt" -nimisen tiedoston SD-kortille ja kirjoittaa sen "Testing 1, 2, 3".

# Syvempi tutkimus:
Historiallinen tausta: Väliaikaisten tiedostojen käyttöä tietokoneohjelmissa on tehty lähes niin kauan kuin tietokoneita on ollut olemassa.

Vaihtoehdot: Voit myös luoda väliaikaisia tiedostoja EEPROM-muistiin tai muulle tallennustilalle.

Toteutuksen yksityiskohdat: Arduino ei sisällä suoraa tukea väliaikaisten tiedostojen luontiin, mutta voit helposti luoda sellaisen avattavalla tiedostolla, kirjoittamalla sen johonkin muotoon ja sitten lopettamalla sen.

# Katso myös:
- Arduino SD-kirjasto: [Arduino-SD Library](https://www.arduino.cc/en/Reference/SD)
- SPIFFS, toinen tapa käsitellä tiedostoja ESP8266/ESP32: [SPIFFS](https://docs.espressif.com/projects/esp-idf/en/latest/esp32/api-reference/storage/spiffs.html)
- EEPROM-kirjasto, jolla on mahdollista luoda muistitiedostoja Arduinossa: [EEPROM Library](https://www.arduino.cc/en/Reference/EEPROM)