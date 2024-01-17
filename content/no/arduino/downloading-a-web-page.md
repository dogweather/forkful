---
title:                "Laste ned en nettside"
html_title:           "Arduino: Laste ned en nettside"
simple_title:         "Laste ned en nettside"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/arduino/downloading-a-web-page.md"
---

{{< edit_this_page >}}

# Hva & Hvorfor?
Nedlasting av en nettside betyr å hente informasjon fra en nettside og bruke den i koden din. Dette er nyttig for å få tilgang til og behandle data fra eksterne kilder. 

Programmerere gjør dette for å få tilgang til og behandle data fra nettsider, som kan brukes til å lage interaktive prosjekter eller hente nyttig informasjon for å forbedre deres kodingsarbeid. 

# Hvordan:
Eksempel på kode for å laste ned en nettside og skrive ut innholdet:
```Arduino
#include <WiFi.h>
#include <WiFiClient.h>
#include <WiFiMulti.h>
#include <HTTPClient.h>

// Vær sikker på å endre SSID og passord til ditt eget nettverk
const char* ssid = "ditt_wifi_navn";
const char* password = "ditt_wifi_passord";

WiFiMulti wifiMulti;
WiFiClient client;
HTTPClient http;

void setup() {
  Serial.begin(115200);

  // Koble til Wi-Fi
  wifiMulti.addAP(ssid, password);
  Serial.println("Kobler til Wi-Fi...");
  while (wifiMulti.run() != WL_CONNECTED) {
    Serial.print(".");
    delay(500);
  }
  Serial.println();
  Serial.println("Koblet til Wi-Fi!");

  // Koble til nettsiden
  http.begin(client, "http://nettsted.no"); // Endre URL-en til nettsiden du vil laste ned
  int httpCode = http.GET();
  if (httpCode > 0) {
    Serial.println("Nettside lastet ned:");
    String payload = http.getString();
    Serial.println(payload);
  }
  http.end();
}

void loop() {
  // Din kode her
}
```

Eksempel på utdata fra koden ovenfor:
```
Kobler til Wi-Fi...
Koblet til Wi-Fi!
Nettside lastet ned:
<!DOCTYPE html>
<html>
<head>
<title>Min nettside</title>
</head>
<body>
<h1>Velkommen til min nettside!</h1>
<p>Her finner du nyttig informasjon om programmering og Arduino.</p>
</body>
</html>
```

# Nærere Undersøk:
## Historisk Kontekst:
Nedlasting av nettsider har blitt mye enklere med utviklingen av Wi-Fi-moduler og biblioteker som støtter HTTP-tilkoblinger. Tidligere måtte programmerere implementere komplekse protokoller som TCP eller UDP for å få tilgang til nettsider.

## Alternativer:
I stedet for å laste ned en hel nettside, kan du også bruke API-er for å få tilgang til spesifikk data på nettsider. Dette kan være mer effektivt og kreve mindre dataoverføring.

## Implementeringsdetaljer:
Denne koden bruker WiFi-, WiFiClient- og HTTPClient-biblioteker for å koble til og laste ned nettsiden. Det er viktig å sørge for at bibliotekene er riktig installert og inkludert i koden din for å unngå feil.

# Se Også:
- [WiFi Biblioteket for Arduino](https://www.arduino.cc/en/Reference/WiFi)
- [HTTP Biblioteket for Arduino](https://arduinogetstarted.com/tutorials/arduino-http-client-library)