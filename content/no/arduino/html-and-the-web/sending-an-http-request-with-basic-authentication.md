---
date: 2024-01-20 18:00:46.388908-07:00
description: "Hvordan gj\xF8re det: Bruk ESP8266 eller ESP32 med WiFi-bibliotek for\
  \ \xE5 koble til et nettverk og utf\xF8re HTTP-foresp\xF8rsler med basisgodkjenning."
lastmod: '2024-03-13T22:44:41.058351-06:00'
model: gpt-4-1106-preview
summary: "Bruk ESP8266 eller ESP32 med WiFi-bibliotek for \xE5 koble til et nettverk\
  \ og utf\xF8re HTTP-foresp\xF8rsler med basisgodkjenning."
title: "\xC5 sende en HTTP-foresp\xF8rsel med grunnleggende autentisering"
weight: 45
---

## Hvordan gjøre det:
Bruk ESP8266 eller ESP32 med WiFi-bibliotek for å koble til et nettverk og utføre HTTP-forespørsler med basisgodkjenning.

```Arduino
#include <ESP8266WiFi.h>
#include <ESP8266HTTPClient.h>

const char* ssid = "ditt_wifi";
const char* password = "ditt_passord";
const char* http_username = "brukernavn";
const char* http_password = "passord";
const char* serverName = "http://ditt.server.com/ressurs";

WiFiClient client;
HTTPClient http;

void setup() {
  Serial.begin(115200);
  WiFi.begin(ssid, password);
  while (WiFi.status() != WL_CONNECTED) {
    delay(1000);
    Serial.println("Kobler til WiFi...");
  }
  Serial.println("WiFi koblet til!");
  http.begin(client, serverName);
  http.setAuthorization(http_username, http_password);
  int httpCode = http.GET();

  if (httpCode > 0) {
    Serial.printf("HTTP Respons Kode: %d\n", httpCode);
    if(httpCode == HTTP_CODE_OK) {
      String payload = http.getString();
      Serial.println(payload);
    }
  } else {
    Serial.printf("Feil ved sending av HTTP: %s\n", http.errorToString(httpCode).c_str());
  }
  http.end();
}

void loop() {
}
```

## Dypdykk
Å sende HTTP-forespørsler med basisgodkjenning er en enkel autentiseringsform som har eksistert siden HTTP/1.0. Til tross for sin enkelhet, er den ikke den sikreste metoden siden brukernavn og passord sendes i klartekst (kodet med Base64, som lett kan dekodes). Derfor anbefales det å bruke HTTPS-forespørsler for å beskytte dataene med SSL/TLS-kryptering. Alternativer til basisgodkjenning inkluderer OAuth, API-nøkler og moderne autentiseringsprotokoller som JWT (JSON Web Token).

## Se Også
- [HTTP-autentisering på MDN Web Docs](https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication)
- [Sikkerhetsvurderinger for HTTP-basisgodkjenning](https://tools.ietf.org/html/rfc7617)
- [HTTPS-protokollen](https://www.arduino.cc/reference/en/libraries/wifi101/)
