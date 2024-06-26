---
date: 2024-01-20 18:00:51.982616-07:00
description: "Hur man g\xF6r: HTTP Basic Authentication introducerades tidigt i webbens\
  \ historia, som en del av HTTP/1.0-specifikationen. Det \xE4r inte det s\xE4kraste\
  \ s\xE4ttet\u2026"
lastmod: '2024-04-05T21:53:39.504843-06:00'
model: gpt-4-1106-preview
summary: HTTP Basic Authentication introducerades tidigt i webbens historia, som en
  del av HTTP/1.0-specifikationen.
title: "Skicka en HTTP-f\xF6rfr\xE5gan med Basic-autentisering"
weight: 45
---

## Hur man gör:
```Arduino
#include <ESP8266WiFi.h>
#include <Base64.h>

const char* ssid = "dittSSID";
const char* password = "dittLösenord";
const char* host = "www.dinserver.com";
const int httpPort = 80;

void setup() {
  Serial.begin(115200);
  WiFi.begin(ssid, password);

  while (WiFi.status() != WL_CONNECTED) {
    delay(500);
    Serial.print(".");
  }
  
  Serial.println("");
  Serial.println("WiFi connected");
  
  String authValue = "Basic " + base64::encode(String("användare:lösenord"));

  WiFiClient client;
  
  if (!client.connect(host, httpPort)) {
    Serial.println("Connection failed");
    return;
  }
  
  String url = "/min_resurs";
  
  client.print(String("GET ") + url + " HTTP/1.1\r\n" +
               "Host: " + host + "\r\n" +
               "Authorization: " + authValue + "\r\n" +
               "Connection: close\r\n\r\n");
  
  while (client.connected()) {
    String line = client.readStringUntil('\n');
    if (line == "\r") {
      break;
    }
  }
  
  String response = client.readStringUntil('\n');
  Serial.println("Server response: ");
  Serial.println(response);
}

void loop() {
  // Gör ingenting här
}
```

## Djupdykning
HTTP Basic Authentication introducerades tidigt i webbens historia, som en del av HTTP/1.0-specifikationen. Det är inte det säkraste sättet att skicka känslig information, eftersom inloggningsuppgifterna enkelt kan avkodas om de inte skickas över HTTPS. Men i ett slutet eller säkert nätverk kan det vara tillräckligt för vissa ändamål. Alternativen inkluderar OAuth, API-nycklar eller JWT-tokens, som erbjuder starkare säkerhet. När det implementeras på en Arduino som använder ESP8266-chipet, som i exemplet ovan, används `Base64`-biblioteket för att koda användarnamn och lösenord. `WiFiClient` används för att upprätta en nätverksanslutning och skicka förfrågan.

## Se även
- ESP8266WiFi biblioteket: https://arduino-esp8266.readthedocs.io/en/latest/esp8266wifi/readme.html
- Base64 kodning i Arduino: https://www.arduino.cc/reference/en/libraries/base64/
- Mer om HTTP-autentisering: https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication
