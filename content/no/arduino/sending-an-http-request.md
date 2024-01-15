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

## Hvorfor
Så du har lyst til å lære hvordan du kan sende en HTTP forespørsel med din Arduino? Vel, det er mange grunner til å gjøre det, men her er noen eksempler: du kan bruke HTTP for å hente data fra en ekstern nettside, sende data til en fjern server eller til og med styre enheter eller applikasjoner via internett.

## Hvordan gjøre det
For å sende en HTTP forespørsel med din Arduino, trenger du først å inkludere WiFi biblioteket. Deretter må du koble din Arduino til et WiFi nettverk ved hjelp av ```WiFi.begin()``` funksjonen. Når du er tilkoblet, kan du nå lage din HTTP forespørsel ved å bruke ```HTTPClient``` biblioteket.

Først må du opprette en HTTP klient med en instans av ```HTTPClient```. Deretter kan du sette URLen du vil sende en forespørsel til ved hjelp av ```HTTPClient.begin()``` funksjonen. Du kan også legge til eventuelle parametere eller data i forespørselen med ```addHeader()``` og ```addParam()``` funksjonene.

Når du er fornøyd med din forespørsel, kan du sende den med ```HTTPClient.GET``` eller ```HTTPClient.POST``` funksjonene, avhengig av hvilken type forespørsel du vil sende. Etter å ha mottatt et svar fra serveren, kan du bruke ```getString()``` funksjonen for å få tilgang til svaret som en ```String```.

```Arduino
#include <WiFi.h>
#include <HTTPClient.h>

WiFiClient wifi;
HTTPClient http;

void setup() {
  // Koble til WiFi nettverk
  WiFi.begin("WiFi-Nettverk", "passord");

  // Opprett HTTP klient og definer URLen
  http.begin(wifi, "http://eksempel.com");

  // Legg til eventuelle parametere og data i forespørselen
  http.addHeader("Content-Type", "application/json");
  http.addParam("temperatur", 25);

  // Send forespørsel og få svar
  int status = http.GET();
  if (status > 0) {
    // Hent ut svaret som en String
    String svar = http.getString();
    Serial.println(svar);
  }
}

void loop() {
  // Gjenta forespørsel hvert minutt
  http.end();
  delay(60000);
}

```

## Dykk ned i det
Som nevnt tidligere, kan du bruke HTTP til å koble din Arduino til internett på flere måter. Det kan være å hente data fra en nettside eller til og med styre eksterne enheter. Du kan også legge til sikkerhetsfunksjoner, som å autentisere deg selv hos serveren før du sender en forespørsel.

En ting å huske på når du jobber med å sende HTTP forespørsler er at det krever at din Arduino har tilgang til internett. Det kan være via et WiFi nettverk eller ved å bruke en Ethernet Shield. Det er også viktig å ha en pålitelig og stabil internettforbindelse for å sikre at dine forespørsler blir sendt og mottatt korrekt.

## Se også
- [WiFi biblioteket for Arduino](https://www.arduino.cc/en/Reference/WiFi)
- [HTTPClient biblioteket for Arduino](https://github.com/zenmanenergy/ESP8266-Arduino-Examples/tree/master/httpRequest)
- [Offisiell Arduino nettside (på norsk)](https://www.arduino.cc/)