---
title:                "Sende en http-forespørsel med grunnleggende autentisering"
html_title:           "Arduino: Sende en http-forespørsel med grunnleggende autentisering"
simple_title:         "Sende en http-forespørsel med grunnleggende autentisering"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/arduino/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

# Hvorfor
HTTP-forespørsler med grunnleggende autentisering er en enkel måte å sikre din kommunikasjon med ulike nettverksenheter. Dette kan for eksempel være nyttig når du ønsker å kommunisere med en API eller en skytjeneste fra dine Arduino-prosjekter. 

# Hvordan
For å sende en HTTP-forespørsel med grunnleggende autentisering i Arduino, trenger du å bruke WiFiClientSecure-biblioteket. Først må du laste ned og installere dette biblioteket fra Arduino IDE ved å gå til "Verktøy" og "Administrer bibliotek". Søk etter "WiFiClientSecure" og last ned det før du følger kodesnuttene nedenfor. 

```Arduino
#include <WiFiClientSecure.h>
//Opprett et WiFiClientSecure-objekt:
WiFiClientSecure client;
//Oppgi server- og portnummeret for forespørsel:
const char* server = "https://www.example.com";
int port = 443;
//Angi brukernavn og passord for grunnleggende autentisering:
const char* username = "example";
const char* password = "password";
//Koble til serveren og sjekk tilkoblingsstatusen:
if(client.connect(server, port)){
  Serial.println("Tilkoblet!");
  //Legg til brukernavn og passord til HTTP-headeren:
  client.println("Authorization: Basic " + String(username) + ":" + String(password) + "\r\n");
  //Send den faktiske HTTP-forespørselen:
  client.println("GET /index.html HTTP/1.1");
  //Les og skriv ut responsen fra serveren:
  while(client.available()){
    String line = client.readStringUntil('\r\n');
    Serial.print(line);
  }
  //Lukk tilkoblingen:
  client.stop();
} else {
  Serial.println("Kunne ikke koble til serveren!");
}
```

Etter å ha kjørt denne koden, bør du se responsen fra serveren i serieporten, og i dette tilfellet vil det være innholdet i "index.html"-siden fra serveren. Du kan gjøre mer komplekse forespørsler ved å endre HTTP-metoden og innholdet eller legge til flere HTTP-header parametere. Se eksemplene i "Deep Dive" seksjonen for mer informasjon om dette. 

# Deep Dive
Når du sender en HTTP-forespørsel med grunnleggende autentisering, er det to viktige ting du må huske på: brukernavn og passord må være i base64-koding og du må inkludere "Authorization" HTTP-headeren i forespørselen. Dette betyr at du trenger å lage en streng som følger dette formatet: "username:password" og konvertere den til base64 før du legger den til i "Authorization" HTTP-headeren. Dette gjøres enkelt ved hjelp av Base64-biblioteket i Arduino. Se et eksempel på koding nedenfor:

```Arduino
#include <Base64.h>
//Opprett en variabel for base64-koding:
String base64encoded = "";
//Kod brukernavn og passord og legg dem til i basen:
base64encoded.concat(username);
base64encoded.concat(":");
base64encoded.concat(password);
//Konverter til base64 og legg til i "Authorization" HTTP-headeren:
client.println("Authorization: Basic " + base64encode.encode(base64encoded) + "\r\n");
```

Du kan også inkludere flere HTTP-header parametere ved å følge samme format, for eksempel:

```Arduino
//Legg til "Content-Type" HTTP-header parametere:
client.println("Content-Type: application/json");
```

Se dokumentasjonen for API-et eller skytjenesten du kommuniserer med for å finne ut hvilke HTTP-header parametere som er nødvendige for en vellykket forespørsel.

# Se også
- [WiFiClientSecure bibliotek](https://github.com/esp8266/Arduino/tree/master/libraries/ESP8266WiFi/examples/HTTPSRequest)
- [Base64 bibliotek](https://www.arduino.cc/en/Reference/Base64)