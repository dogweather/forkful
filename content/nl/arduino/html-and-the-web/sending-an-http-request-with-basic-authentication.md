---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:07:53.636093-07:00
description: "Een HTTP-verzoek verzenden met basisauthenticatie voegt een beveiligingslaag\
  \ toe door om een gebruikersnaam en wachtwoord te vragen. Programmeurs\u2026"
lastmod: '2024-03-13T22:44:51.072395-06:00'
model: gpt-4-0125-preview
summary: "Een HTTP-verzoek verzenden met basisauthenticatie voegt een beveiligingslaag\
  \ toe door om een gebruikersnaam en wachtwoord te vragen. Programmeurs\u2026"
title: Een HTTP-verzoek verzenden met basisauthenticatie
weight: 45
---

## Wat & Waarom?
Een HTTP-verzoek verzenden met basisauthenticatie voegt een beveiligingslaag toe door om een gebruikersnaam en wachtwoord te vragen. Programmeurs gebruiken dit om toegang te krijgen tot API's of webservices die alleen toegankelijk zijn voor geautoriseerde gebruikers.

## Hoe:
Om dit op een Arduino te realiseren, moet je eerst de benodigde bibliotheken opnemen - typisch `<ESP8266WiFi.h>` voor ESP8266 of `<WiFi.h>` voor ESP32, en `<Base64.h>` voor het coderen van authenticatiegegevens. Hier is een minimalistische codefragment om je op weg te helpen:

```Arduino
#include <ESP8266WiFi.h>
#include <Base64.h>

const char* ssid = "jeSSID";
const char* wachtwoord = "jeWachtwoord";
const char* server = "jouw.server.com";
const char* authGebruiker = "gebruiker";
const char* authWachtwoord = "wachtwoord";

void setup() {
  Serial.begin(115200);
  WiFi.begin(ssid, wachtwoord);

  while (WiFi.status() != WL_CONNECTED) {
    delay(500);
    Serial.print(".");
  }
  
  String auth = "Basic " + base64::encode(String(authGebruiker) + ":" + String(authWachtwoord));

  WiFiClient client;
  if (client.connect(server, 80)) {
    client.println("GET /route HTTP/1.1");
    client.print("Host: ");
    client.println(server);
    client.println("Authorization: " + auth);
    client.println("Connection: close");
    client.println();
  }
}

void loop() {
  // Jouw reguliere code hier
}
```

Na het uitvoeren zal de Arduino verbinden met de opgegeven server met de inloggegevens en de beveiligde inhoud ophalen.

## Diepere duik

HTTP Basisauthenticatie bestaat al sinds de vroege dagen van het web, gedefinieerd in 1996 door RFC 2617. Het is eenvoudig: codeer gebruikersnaam en wachtwoord in base64 en plaats het op een HTTP-header. Het is niet de meest veilige methode (omdat base64 gemakkelijk omkeerbaar is), maar het is eenvoudig voor laag-risicosituaties of interne tools.

Er zijn alternatieven, zoals Digest Access Authentication of OAuth, die veiliger zijn, maar ze zijn ook zwaarder qua bronnen - iets om te overwegen op een kleine Arduino.

Voor implementatie, houd er rekening mee dat base64-codering de grootte van de inloggegevens met ongeveer 33% verhoogt en het geheugen van Arduino is beperkt. Zorg er ook voor dat je server SSL/TLS (HTTPS) gebruikt als je inloggegevens over het internet verzendt om blootstelling te voorkomen.

## Zie ook
- [Wikipedia over Basis toegangsauthenticatie](https://en.wikipedia.org/wiki/Basic_access_authentication)
- [Beveilig je HTTP-verzoek](https://arduino.cc/en/Tutorial/WebClientRepeating)
