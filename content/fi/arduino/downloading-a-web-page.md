---
title:                "Verkkosivun lataaminen"
html_title:           "Arduino: Verkkosivun lataaminen"
simple_title:         "Verkkosivun lataaminen"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/arduino/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Mikä ja miksi?

Lataaminen ("downloading") tarkoittaa verkkosivun tallentamista internetistä omalle laitteelle. Ohjelmoijat käyttävät tätä toimintoa esimerkiksi hakeakseen tietoa erilaisilta sivustoilta, kuten säätiedoista tai uutisista.

## Kuinka:

```
ArduinoWiFiClient client;
// Avataan yhteys url-osoitteeseen
client.connect("www.example.com", 80);
// Lähetetään HTTP-pyyntö
client.println("GET /index.html HTTP/1.1");
client.println("Host: www.example.com");
client.println("Connection: close");
client.println();
// Luetaan vastaus ja tallennetaan se muuttujaan
String response = client.readString();
// Tulostetaan vastaus sarjamonitorille
Serial.println(response);
// Suljetaan yhteys
client.stop();
```

*Lopullinen tuloste:*

```
HTTP/1.1 200 OK
Date: Thu, 25 Feb 2021 00:00:00 GMT
Server: Apache
Last-Modified: Mon, 18 Jan 2021 00:00:00 GMT
ETag: "123abc456"
Accept-Ranges: bytes
Content-Length: 2911
Connection: close
Content-Type: text/html

<!DOCTYPE html>
<html>
<head><title>Esimerkkisivu</title></head>
<body>
Tervetuloa esimerkkisivulle!
</body>
</html>
```

## Syvemmät vedet:

Lataamista on käytetty jo pitkään eri ohjelmointikielillä. Arduino-kirjastoissa on käytettävissä erilaisia vaihtoehtoja lataamiseen, kuten WiFiClient ja EthernetClient. Myös ulkoiset kirjastot, kuten ESP8266WiFi, tarjoavat lisämahdollisuuksia.

## Katso myös:

- [WiFiClient - Arduino Reference](https://www.arduino.cc/en/Reference/WiFiClient)
- [ESP8266WiFi Library - GitHub](https://github.com/esp8266/Arduino/tree/master/libraries/ESP8266WiFi)