---
title:                "HTTP-pyynnön lähettäminen"
html_title:           "Bash: HTTP-pyynnön lähettäminen"
simple_title:         "HTTP-pyynnön lähettäminen"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/arduino/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Mikä & Miksi?

HTTP-pyyntö on tapa, jolla laitteet kommunikoivat verkossa. Se on välttämätöntä tietojen vaihtamiseen palvelimien ja asiakasohjelmistojen, kuten Arduino-laitteen, välillä.

## Miten toimii:

Tässä esimerkki siitä, kuinka Arduino-laitteesi voi lähettää HTTP GET -pyynnön:

```Arduino
#include <Ethernet.h>
#include <SPI.h>

byte mac[] = { 0xDE, 0xAD, 0xBE, 0xEF, 0xFE, 0xED };
char server[] = "www.sivustosi.com";
EthernetClient client;

void setup() {
  Serial.begin(9600);
  Ethernet.begin(mac);
  delay(1000);
  
  if (client.connect(server, 80)) {
    client.println("GET /index.html HTTP/1.1");
    client.println("Host: www.sivustosi.com");
    client.println("Connection: close");
    client.println();
  }
}

void loop() {
  if (client.available()) {
    char c = client.read();
    Serial.print(c);
  }
  
  if (!client.connected()) {
    client.stop();
    for(;;);
  }
}
```

## Syvällisemmin:

HTTP-pyynnöt tulivat internetiin 1990-luvun alussa HTTP-protokollan myötä. Vaihtoehtoina on monia muita tapoja tehdä verkkopyyntöjä, kuten POST, PUT ja DELETE.

Mitä tulee toteutukseen, Arduino ylläpitää avoimen standardin Ethernet-kirjastoa, joka helpottaa HTTP-pyyntöjen tekemistä. Yllä olevassa koodissa, liitämme ensin EthernetClient-olioon, ja lähetämme sitten standardin GET-pyynnön sisältäen palvelimen nimen ja resurssin polun. 

## Lisätietoa:

1. [Arduinon Ethernet-kirjasto](https://www.arduino.cc/en/Reference/Ethernet)
2. [HTTP-pyynnöt selitetty](https://www.tutorialspoint.com/http/http_requests.htm)
3. [HTTP:n historiaa](https://www.lifewire.com/history-of-http-817945)