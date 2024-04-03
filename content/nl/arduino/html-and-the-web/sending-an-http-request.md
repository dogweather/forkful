---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:07:47.606435-07:00
description: "Een HTTP-verzoek verzenden is de manier waarop je Arduino met het web\
  \ praat, zoals een server vragen om wat gegevens terug te sturen. Programmeurs doen\u2026"
lastmod: '2024-03-13T22:44:51.069529-06:00'
model: gpt-4-0125-preview
summary: Een HTTP-verzoek verzenden is de manier waarop je Arduino met het web praat,
  zoals een server vragen om wat gegevens terug te sturen.
title: Een HTTP-verzoek verzenden
weight: 44
---

## Hoe:
Werken met de Arduino vereist de `WiFiNINA`-bibliotheek voor netwerkfuncties. Hier is hoe je een eenvoudig GET-verzoek verzendt:

```Arduino
#include <WiFiNINA.h>

char ssid[] = "jeNetwerkNaam";       // je netwerk SSID (naam)
char pass[] = "jeNetwerkWachtwoord";       // je netwerkwachtwoord
int status = WL_IDLE_STATUS;           // de status van de WiFi-radio
char server[] = "voorbeeld.com";         // server waarmee je wilt verbinden

WiFiClient client;

void setup() {
  Serial.begin(9600);                  // start serieel voor debuggen
  WiFi.begin(ssid, pass);              // start de WiFi-verbinding
  while (status != WL_CONNECTED) {     // wacht op de verbinding:
    status = WiFi.status();
    delay(1000);
  }
  Serial.print("Verbonden met ");
  Serial.println(ssid);
}

void loop() {
  if (client.connect(server, 80)) {    // als je een verbinding krijgt, verzend het verzoek:
    client.println("GET / HTTP/1.1");
    client.println("Host: voorbeeld.com");
    client.println("Connection: close");
    client.println();                   // einde van het verzoek
  } else {
    Serial.println("Verbinding mislukt"); // als je geen verbinding met de server krijgt:
  }

  while (client.connected()) {         // terwijl je verbonden bent, lees de gegevens:
    if (client.available()) {
      char c = client.read();
      Serial.print(c);
    }
  }

  if (!client.connected()) {           // als de server is losgekoppeld, stop de client:
    client.stop();
  }

  delay(10000);                        // wacht tien seconden voor je het opnieuw probeert
}
```

Voorbeeldoutput:
```
HTTP/1.1 200 OK
Datum: Ma, 23 Jan 2023 12:36:47 GMT
Server: Apache/2.4.1 (Unix)
...
```

## Diepe Duik
Het concept van het verzenden van een HTTP-verzoek vanaf een microcontroller was niet altijd een ding. In het verleden waren microcontrollers meer over sensoren en interactie met de fysieke wereld. Maar met de opkomst van het IoT (Internet of Things) begonnen deze apparaten webconnectiviteit nodig te hebben. De Arduino kan nu bibliotheken zoals `WiFiNINA` gebruiken om deze verbindingen robuust te beheren.

Alternatieven voor `WiFiNINA` bestaan afhankelijk van je hardware. Bijvoorbeeld, de `Ethernet`-bibliotheek maakt gebruik van bedrade verbindingen, terwijl `WiFi101` werkt met oudere WiFi-shields.

Aan de implementatiekant lijkt het maken van een HTTP-verzoek eenvoudig, maar de handshake, headers en HTTP-methoden (GET, POST, etc.) zijn onderdeel van een strikt protocol dat apparaten in staat stelt om via het web te communiceren. De Arduino abstraheert veel van deze complexiteit, maar het begrijpen van de basis helpt bij het oplossen van problemen wanneer dingen niet soepel verlopen.

## Zie Ook
- Arduino `WiFiNINA` bibliotheekdocumentatie: https://www.arduino.cc/en/Reference/WiFiNINA
- HTTP-protocol introductie: https://developer.mozilla.org/en-US/docs/Web/HTTP
- Arduino projecthub voor webverbonden projecten: https://create.arduino.cc/projecthub
