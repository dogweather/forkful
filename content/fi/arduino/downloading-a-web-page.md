---
title:                "Verkkosivun lataaminen"
html_title:           "C#: Verkkosivun lataaminen"
simple_title:         "Verkkosivun lataaminen"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/arduino/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Mitä & Miksi?

Web-sivun lataaminen on prosessi, jossa tiedot haetaan verkkosivulta paikalliseen laitteeseen. Ohjelmoijat tekevät tämän tiedon hankkimiseksi tai tiettyjen tehtävien, kuten web-sisällön skannauksen, automatisoimiseksi.

## Näin se tehdään:

Arduino-koodin käyttämällä voimme ladata web-sivun. Esimerkki alla:

```Arduino
#include <ESP8266WiFi.h>

const char* ssid     = "your_SSID";
const char* password = "your_PASSWORD";

const char* host = "your_webhost.com"; 

void setup() {
  Serial.begin(115200);
  delay(10);

  // Yhdistetään WiFi-verkkoon
  Serial.println();
  Serial.println();
  Serial.print("Yhdistetään: ");
  Serial.println(ssid);
  
  WiFi.begin(ssid, password);
  
  while (WiFi.status() != WL_CONNECTED) {
    delay(500);
    Serial.print(".");
  }

  Serial.println("");
  Serial.println("WiFi yhdistetty");  
  Serial.println("IP osoite: ");
  Serial.println(WiFi.localIP());
}

void loop() {
  delay(10000);

  Serial.print("Yhdistetään: ");
  Serial.println(host);

  // Käytä WiFiClient-luokkaa yhteyden luomiseksi.
  WiFiClient client;

  const int httpPort = 80;
  if (!client.connect(host, httpPort)) {
    Serial.println("Yhteys epäonnistui");
    return;
  }

  // Lähetämme HTTP GET -pyynnön:
  client.print(String("GET ") + "/path-to-webpage" + " HTTP/1.1\r\n" +
               "Host: " + host + "\r\n" + 
               "Connection: close\r\n\r\n");
               
  while(client.connected()){
    String line = client.readStringUntil('\r');
    Serial.print(line);
  }
}
```

Koodi luo WiFi-yhteyden, yhdistää määritettyyn isäntään ja lähettää HTTP GET -pyynnön isännälle. Se tulostaa vastauksen sarjamonitoriin.

## Deep Dive

Historiallisesti web-sivun lataaminen aloitettiin tekstipohjaisen selaimen avulla, joka kävi läpi HTML-koodin. Vastaavasti Arduino-ohjelma käy läpi HTML-tekstin ja voidaan määrittää suorittamaan tehtäviä tuloksen perusteella.

Web-sivujen lataamiseen on useita vaihtoehtoja eri ohjelmointikielillä, esimerkiksi Python-kieli käyttää `requests`-kirjastoa. Käytetty metodi riippuu laitteesta ja käytetyistä käytännöistä. Arduino-käyttäjät yleensä käyttävät ESP8266WiFi-kirjastoa.

## Katso myös

1. [Arduino virallinen kotisivu](https://www.arduino.cc/)
2. [ESP8266WiFi kirjaston dokumentaatio](https://arduino-esp8266.readthedocs.io/en/latest/esp8266wifi/readme.html)
3. [HTTP GET pyyntöjen opas](https://www.w3schools.com/tags/ref_httpmethods.asp)
4. [Arduino langaton kommunikaatio](https://www.arduino.cc/en/Guide/ArduinoWirelessShield)