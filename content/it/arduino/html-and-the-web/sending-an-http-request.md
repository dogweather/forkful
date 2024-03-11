---
date: 2024-01-20 17:58:55.544615-07:00
description: Inviamo una richiesta HTTP per comunicare con server web. Usato per ottenere
  informazioni da servizi internet o inviarne.
lastmod: '2024-03-11T00:14:17.299121-06:00'
model: gpt-4-1106-preview
summary: Inviamo una richiesta HTTP per comunicare con server web. Usato per ottenere
  informazioni da servizi internet o inviarne.
title: Inviare una richiesta http
---

{{< edit_this_page >}}

## What & Why?
Inviamo una richiesta HTTP per comunicare con server web. Usato per ottenere informazioni da servizi internet o inviarne.

## How to:
Connettiti alla rete, configura l'HTTPS, invia la richiesta:

```Arduino
#include <WiFi.h>
#include <HTTPClient.h>

const char* ssid = "il_tuo_ssid";
const char* password = "la_tua_password";

void setup() {
  Serial.begin(115200);
  WiFi.begin(ssid, password);

  while (WiFi.status() != WL_CONNECTED) {
    delay(1000);
    Serial.println("Connesso alla rete WiFi...");
  }

  HTTPClient http;
  http.begin("http://api.example.com/data");
  int httpCode = http.GET();

  if (httpCode > 0) {
    String payload = http.getString();
    Serial.println(httpCode);
    Serial.println(payload);
  } else {
    Serial.println("Errore nella richiesta");
  }
  http.end();
}

void loop() {
}
```
Output:
```
200
{"chiave":"valore"}
```

## Deep Dive
Invio HTTP nasce con il web. Usiamo microcontrollori come ESP8266 o ESP32 per IoT. Alternativa ad HTTPS è HTTP, meno sicuro ma più semplice.

Dettagli: 
- `WiFi.h` per connessione Wi-Fi.
- `HTTPClient.h` per protocollo HTTP.
- Uso di `begin` per aprire connessione.
- `GET` per richiesta dati da server (esiste anche `POST`, `PUT`, ecc.).
- Codici risposta HTTP esprimono stato (200 OK, 404 Non Trovato, ecc.).
- Sicurezza in HTTPS è critica in comunicazione sensibile.

## See Also
- [La documentazione ufficiale di Arduino per il WiFi](https://www.arduino.cc/en/Reference/WiFi)
- [Guida approfondita su HTTP](https://developer.mozilla.org/it/docs/Web/HTTP/Overview)
- [Confronto tra HTTP e HTTPS](https://www.instantssl.com/http-vs-https)
- [ESP32 come stazione WiFi](https://randomnerdtutorials.com/esp32-useful-wi-fi-functions-arduino/)
