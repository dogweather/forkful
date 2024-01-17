---
title:                "Lähettäminen http-pyyntöä"
html_title:           "Arduino: Lähettäminen http-pyyntöä"
simple_title:         "Lähettäminen http-pyyntöä"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/arduino/sending-an-http-request.md"
---

{{< edit_this_page >}}

# Mitä ja miksi?
Lähetettäessä HTTP-pyyntö, Arduino saa tai lähettää tietoja internetissä oleville palvelimille, kuten verkkosivuille tai sovelluksille. Ohjelmoijat käyttävät tätä ominaisuutta esimerkiksi IoT-projekteissa, jossa tietoja täytyy lähettää tai vastaanottaa internetin kautta.

# Kuinka tehdä?
Alla on kaksi esimerkkiä koodista, jolla voit lähettää HTTP-pyynnön ja nähdä vastauksen Serial Monitor-työkalussa.

```Arduino
#include <WiFi.h>

const char* ssid = "WIFI_NIMI";
const char* password = "WIFI_SALASANA";

void setup() {
  Serial.begin(9600);

  WiFi.begin(ssid, password);

  while (WiFi.status() != WL_CONNECTED) {
    delay(1000);
    Serial.println("Yritetään muodostaa yhteyttä...");
  }
}

void loop() {
  WiFiClient client;

  if (client.connect("osoite.com", 80)) {
    Serial.println("Yhteys muodostettu!");
    client.println("GET /polku HTTP/1.1");
    client.println("Host: osoite.com");
    client.println("Connection: close");
    client.println();
  } else {
    Serial.println("Yhteyden muodostaminen epäonnistui.");
  }

  while (client.available()) {
    String line = client.readStringUntil('\r');
    Serial.print(line);
  }

  client.stop();
  delay(5000);
}
```

Odotettu tulos:
```
Yritetään muodostaa yhteyttä...
Yhteys muodostettu!
HTTP/1.1 200 OK
Server: nginx/1.14.0
Date: Mon, 01 Mar 2021 18:00:00 GMT
Content-Length: 6
Connection: close

Terve!
```

# Syvemmälle
HTTP-pyyntöjen lähettäminen on ollut tärkeä osa internetin kehittymistä, sillä se mahdollistaa tietojen välittämisen eri puolilta maailmaa. Arduinon lisäksi pyyntöjä voidaan lähettää myös muilla laitteilla, kuten tietokoneilla tai mobiililaitteilla. Lisäksi on olemassa muita protokollia, kuten MQTT, joilla voidaan lähettää ja vastaanottaa tietoa internetin kautta.

# Katso myös
- [Arduino WiFi Client-kirjasto](https://www.arduino.cc/en/Reference/WiFiClient)
- [Arduino WiFi-yhteysohjeet](https://www.arduino.cc/en/Guide/ArduinoWiFiNINA)
- [HTTP-pyyntöjen perusteet](https://developer.mozilla.org/fi/docs/Web/HTTP/Overview)