---
title:                "Sende en http-forespørsel med grunnleggende autentisering"
html_title:           "Kotlin: Sende en http-forespørsel med grunnleggende autentisering"
simple_title:         "Sende en http-forespørsel med grunnleggende autentisering"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/arduino/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Hva og Hvorfor?
Å sende en HTTP-forespørsel med grunnleggende autentisering innebærer å overføre data via en Internett-protokoll med sikkerhetstiltak. Dette er nyttig for å beskytte sensitiv informasjon mot uautorisert tilgang.

## Hvordan gjøre det:
Her viser vi hvordan vi sender en HTTP GET forespørsel med Basic Authentication ved bruk av ESP8266WiFi-biblioteket. Installer og importer bibliotekene så:
```Arduino
#include <ESP8266WiFi.h>
#include <ESP8266HTTPClient.h>
```
Koble til Wi-Fi nettverket ditt:
```Arduino
// Erstatt med dine Wi-Fi detaljer
const char* ssid     = "ditt SSID";
const char* password = "din passord";

WiFi.begin(ssid, password);
while (WiFi.status() != WL_CONNECTED) {
    delay(1000);
    Serial.println("Connecting to WiFi...");
}
Serial.println("Connected to WiFi");
```
Nå er du klar til å lage HTTP-forespørselen med grunnleggende autentisering:
```Arduino
HTTPClient http;
http.begin("http://example.com");
http.setAuthorization("username","password");

int httpCode = http.GET();
String payload = http.getString();
http.end();
```
Du vil nå ha HTTP-koden og svaret lagret i `httpCode` og `payload`, henholdsvis.

## Dypdykk
Historisk sett har HTTP basic autentisering vært for å gi en enkel metode for beskyttelse mot inntrengere. Det bør imidlertid ikke være det eneste sikkerhetstiltaket man tar i bruk på grunn av manglende kryptering.

Alternativene inkluderer Digest Access Authentication, en litt sikrere metode, og OAuth, en mye mer sikker standard som brukes av mange moderne web APIs.

Koden ovenfor bruker Base64-koding for å lage 'authorization'-headeren. Det er viktig å merke seg at base64-koding ikke betyr kryptering - det er bare en måte å reformatere bytes på.

## Se også
- Arduino HTTP Client Library: https://github.com/arduino-libraries/ArduinoHttpClient
- Basic access authentication, Wikipedia: https://en.wikipedia.org/wiki/Basic_access_authentication
- Digest access authentication, Wikipedia: https://en.wikipedia.org/wiki/Digest_access_authentication
- OAuth, Wikipedia: https://en.wikipedia.org/wiki/OAuth