---
title:                "Å sende en http-forespørsel"
html_title:           "Arduino: Å sende en http-forespørsel"
simple_title:         "Å sende en http-forespørsel"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/arduino/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?

Å sende en HTTP-forespørsel betyr rett og slett å be om informasjon fra en nettside eller webserver. Dette er en viktig del av de fleste programmeringsprosjekter, da det tillater å integrere data fra ulike kilder og få tilgang til informasjon på en enkel måte.

## Hvordan:

For å sende en HTTP-forespørsel med Arduino, kan du bruke biblioteket "HTTPClient.h". Her er et eksempel på hvordan du kan be om informasjon fra en nettside:

```Arduino
#include <WiFi.h>
#include <HTTPClient.h>

void setup() {
  Serial.begin(9600);
  WiFi.begin("navn-på-wifi-nettverk", "passord");
  while (WiFi.status() != WL_CONNECTED) {
    delay(500);
  }
  Serial.println("Koblet til internett!");
  
  HTTPClient http;

  http.begin("www.example.com"); // Erstatt med nettstedet du ønsker å få informasjon fra
  int httpCode = http.GET();    // Sender GET-forespørselen

  if (httpCode > 0) { // Sjekker om forbindelsen var vellykket
    String payload = http.getString(); // Leser dataene som ble returnert fra nettstedet
    Serial.println(payload); // Skriver ut dataene i seriell monitor
  }
  http.end(); // Avslutter forbindelsen
}

void loop() {
  // Ingenting å gjøre her, kode som trenger å kjøre i loop plasseres i setup-funksjonen
}
```

## Deep Dive:

HTTP (Hypertext Transfer Protocol) er et protokoll for å kommunisere mellom klienter og servere på internett. Først og fremst brukt til å be om og sende HTML-sider, men nå brukt til å overføre en rekke forskjellige dataformater.

HTTP-forespørsler kan gjøres gjennom ulike metoder som GET, POST, PUT, DELETE, osv. Avhengig av hva slags handling som ønskes utført. Det finnes også ulike metoder for autentisering for å sikre at dataene blir overført sikkert.

Alternativer til å bruke "HTTPClient.h" biblioteket inkluderer å implementere en TCP/IP-forbindelse for å sende en GET-forespørsel og behandle dataene manuelt. Dette gir større kontroll, men krever også mer avansert programmering.

## Se også:

- [HTTPClient.h biblioteket dokumentasjon](https://arduinojson.org/)
- [HTTP-forespørsler med Arduino og NodeMCU](https://www.hackster.io/electropeak/http-request-with-arduino-nodemcu-using-httpclient-library-08b367)
- [Mer om HTTP-protokollen og dens ulike metoder](https://developer.mozilla.org/en-US/docs/Web/HTTP)