---
title:                "Arduino: Tiedoston lukeminen"
simple_title:         "Tiedoston lukeminen"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/arduino/reading-a-text-file.md"
---

{{< edit_this_page >}}

# Miksi

Tervetuloa lukemaan Arduino-ohjelmointi blogitekstiä, joka käsittelee tekstitiedoston lukemista! Tekstitiedostojen lukeminen voi olla hyödyllistä monista syistä, kuten tiedostojen tallentamisesta laitteeseen, datan keräämiseen tai jopa pelien tallentamiseen. Jatka lukemista, jos haluat oppia lisää.

# Kuinka

Tekstitiedoston lukeminen Arduinoilla on helppoa. Voit käyttää SD-korttia tai jopa ESP8266 WiFi-moduulia lukeaksesi tekstitiedostoja.

#### SD-Kortti:
```
#include <SPI.h>
#include <SD.h>

File tiedosto;

void setup() {
  Serial.begin(9600);

  // Asenna SD-kortti
  if (!SD.begin(4)) {
    Serial.println("SD-korttia ei löydetty");
    while (1);
  }
  Serial.println("SD-kortti löydetty");
  
  // Avaa tiedosto
  tiedosto = SD.open("tiedosto.txt");
  
  // Tulosta tiedoston sisältö sarjamonitorille
  while (tiedosto.available()) {
    Serial.write(tiedosto.read());
  }
  
  // Sulje tiedosto
  tiedosto.close();
}

void loop() {

}
```
Tämän koodin avulla voit lukea ja tulostaa tekstitiedoston sisällön sarjamonitorille.

#### ESP8266 WiFi-moduuli:
```
#include <ESP8266WiFi.h>
#include <WiFiClient.h>

void setup() {
  Serial.begin(9600);
  
  // Yhdistä WiFi-verkkoon
  WiFi.begin("verkon_nimi", "salasana");
  
  // Odota yhdistämistä
  while (WiFi.status() != WL_CONNECTED) {
    delay(500);
  }
  
  Serial.println("Yhdistetty WiFi-verkkoon");
  
  // Luo WiFi-asiakas
  WiFiClient asiakas;
  
  // Avaa yhteys palvelimeen
  if (asiakas.connect("www.example.com", 80)) {
    // Lähetä HTTP-pyyntö
    asiakas.print("GET /tiedosto.txt HTTP/1.1\r\n");
    asiakas.print("Host: www.example.com\r\n");
    asiakas.print("Connection: close\r\n\r\n");
    
    // Lue vastaus
    while (asiakas.available()) {
      String rivi = asiakas.readStringUntil('\n');
      Serial.println(rivi);
    }
  }
  
  // Sulje yhteys
  asiakas.stop();
}

void loop() {

}
```
Tämä koodi yhdistää WiFi-verkkoon ja lukee tekstitiedoston verkosta ja tulostaa sen sisällön sarjamonitorille.

# Syväsukellus

Tekstitiedoston lukeminen voi olla hieman monimutkaisempaa, jos siinä on erikoismerkkejä tai numerosarjoja. Voit kuitenkin käyttää esimerkiksi String-luokkaa tai strtok-funktiota selvittääksesi, miten käsitellä tiedoston sisältöä. Voit myös käyttää Serial.readStringUntil(f()-funktio päätymisen merkille.
```
#include <SPI.h>
#include <SD.h>

File tiedosto;

void setup() {
  Serial.begin(9600);

  // Asenna SD-kortti
  if (!SD.begin(4)) {
    Serial.println("SD-korttia ei löydetty");
    while (1);
  }
  Serial.println("SD-kortti löydetty");
  
  // Avaa tiedosto
  tiedosto = SD.open("tiedosto.txt");
  
  // Lue tiedosto rivi kerrallaan ja tulosta sarjamonitorille.
  while (tiedosto.available()) {
    String rivi = tiedosto.readStringUntil('\n');
    Serial.println(rivi);
  }
  
  // Sulje tiedosto
  tiedosto.close();
}

void loop() {

}
```

# Katso myös

- [Arduino SD Library](https://www.arduino.cc/en/reference/SD)
- [ESP8266WiFi Library](https://github.com/esp8266/Arduino/tree/master/libraries/ESP8266WiFi)