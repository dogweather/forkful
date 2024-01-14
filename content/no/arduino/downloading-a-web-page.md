---
title:                "Arduino: Nedlasting av en nettside"
simple_title:         "Nedlasting av en nettside"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/arduino/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Hvorfor
Hvorfor vil noen laste ned en nettside ved hjelp av en Arduino? Årsakene kan være mange, men de vanligste inkluderer å kunne kontrollere og manipulere en nettside for å få informasjon eller å automatisere en prosess.

## Hvordan
For å laste ned en nettside på Arduino, må du først inkludere WiFi biblioteket ved å skrive ```#include <WiFi.h>```. Deretter må du definere variabler for SSID (nettverksnavnet) og passordet til Wi-Fi nettverket ditt.
```
const char *ssid = "Navnet på ditt Wi-Fi nettverk";
const char *password = "Passordet ditt";
```
Videre må du opprette en forbindelse til Wi-Fi ved å bruke følgende kode:
```
WiFi.begin(ssid, password);
while (WiFi.status() != WL_CONNECTED) {
    delay(500);
}
```
Når du er koblet til Wi-Fi, kan du bruke HTTP klient biblioteket ved å skrive ```#include <HTTPClient.h>```.
Nå kan du bruke ```HTTPClient``` objektet til å laste ned en nettside med ```begin()```, ```GET()``` og ```getString()``` funksjonene.
```
HTTPClient http;
http.begin("http://www.example.com");
String response = http.getString();
```
Endelig må du frigjøre ressursene ved å kalle ```http.end()```.

## Dypdykk
En ting å merke seg er at dette bare fungerer hvis nettsiden bruker HTTP og ikke HTTPS. Hvis nettsiden bruker HTTPS, vil du måtte bruke et SSL bibliotek i tillegg.
En annen viktig ting å huske på er at Arduino har begrenset minne, så det kan være lurt å ikke laste ned for store nettsider eller å begrense mengden data du henter ved å bruke ```writeToStream()``` funksjonen.

## Se Også
- [Arduino WiFi bibliotek](https://www.arduino.cc/en/Reference/WiFi)
- [HTTP Client biblioteket](https://github.com/arduino-libraries/ArduinoHttpClient)
- [SSL biblioteket](https://github.com/arduino-libraries/ArduinoBearSSL)