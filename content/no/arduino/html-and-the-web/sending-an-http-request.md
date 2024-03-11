---
date: 2024-01-20 17:58:56.301342-07:00
description: "En HTTP-foresp\xF8rsel lar en Arduino snakke med internett - den sp\xF8\
  r, og et server svarer. Vi gj\xF8r dette for \xE5 sl\xE5 opp data, styre ting p\xE5\
  \ avstand, eller\u2026"
lastmod: '2024-03-11T00:14:14.645665-06:00'
model: gpt-4-1106-preview
summary: "En HTTP-foresp\xF8rsel lar en Arduino snakke med internett - den sp\xF8\
  r, og et server svarer. Vi gj\xF8r dette for \xE5 sl\xE5 opp data, styre ting p\xE5\
  \ avstand, eller\u2026"
title: "\xC5 sende en HTTP-foresp\xF8rsel"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
En HTTP-forespørsel lar en Arduino snakke med internett - den spør, og et server svarer. Vi gjør dette for å slå opp data, styre ting på avstand, eller logge informasjon online.

## Slik gjør du:
Arduino-kode for å sende en enkel GET-forespørsel:

```Arduino
#include <WiFi.h>
const char* ssid = "dittNettverksnavn";
const char* password = "dittPassord";
const char* host = "www.eksempel.com";

void setup() {
  Serial.begin(115200);
  WiFi.begin(ssid, password);
  while (WiFi.status() != WL_CONNECTED) {
    delay(500);
    Serial.print(".");
  }
  Serial.println("WiFi tilkoblet.");
  Serial.print("IP-adresse: ");
  Serial.println(WiFi.localIP());

  WiFiClient client;
  if (!client.connect(host, 80)) {
    Serial.println("Tilkobling feilet");
    return;
  }
  
  client.print(String("GET ") + "/sti/til/ressurs" + " HTTP/1.1\r\n" +
               "Host: " + host + "\r\n" + 
               "Connection: close\r\n\r\n");
  unsigned long timeout = millis();
  while (client.available() == 0) {
    if (millis() - timeout > 5000) {
      Serial.println(">>> Timeout!");
      client.stop();
      return;
    }
  }
  while(client.available()) {
    String line = client.readStringUntil('\r');
    Serial.print(line);
  }
}

void loop() {
}
```
Eksempel-output når alt har gått bra:
```
WiFi tilkoblet.
IP-adresse: 192.168.1.4
HTTP/1.1 200 OK
...
```

## Dybdedykk
Å sende HTTP-forespørsler er essensielt for tingenes internett (IoT). På 90-tallet begynte webklienter å be om data fra servere, noe som drev fremveksten av det moderne internett. Alternativene til HTTP inkluderer MQTT for lette meldinger i IoT eller WebSockets for kontinuerlig kommunikasjon. Med Arduino, er utfordringen ofte i detaljene – å sikre stabile nettverksforbindelser, effektiv kode, og sikkerhetsaspekter som SSL/TLS.

## Se også
- [ESP8266WiFi library for spesifikke instruksjoner for ESP8266-moduler](https://github.com/esp8266/Arduino)
- [MQTT for Arduino: et alternativ for IoT-kommunikasjon](https://pubsubclient.knolleary.net/)
