---
date: 2024-01-20 17:43:23.364156-07:00
description: "Scaricare una pagina web significa far s\xEC che il tuo Arduino la richieda\
  \ e la riceva dalla rete. I programmatori lo fanno per acquisire dati da internet,\u2026"
lastmod: '2024-02-25T18:49:41.542033-07:00'
model: gpt-4-1106-preview
summary: "Scaricare una pagina web significa far s\xEC che il tuo Arduino la richieda\
  \ e la riceva dalla rete. I programmatori lo fanno per acquisire dati da internet,\u2026"
title: Scaricare una pagina web
---

{{< edit_this_page >}}

## Cosa & Perché?
Scaricare una pagina web significa far sì che il tuo Arduino la richieda e la riceva dalla rete. I programmatori lo fanno per acquisire dati da internet, come il meteo, o per controllare una pagina remotamente.

## Come fare:
```Arduino
#include <WiFi.h>
#include <HTTPClient.h>

const char* ssid = "il_tuo_SSID";
const char* password = "la_tua_password";

void setup() {
  Serial.begin(115200);
  WiFi.begin(ssid, password);

  while (WiFi.status() != WL_CONNECTED) {
    delay(1000);
    Serial.println("Connessione al WiFi...");
  }
  
  HTTPClient http;
  http.begin("http://esempio.com/pagina.html");
  int httpCode = http.GET();

  if (httpCode > 0) {
    String payload = http.getString();
    Serial.println(payload);
  } else {
    Serial.println("Errore nella ricezione della pagina");
  }
  
  http.end();
}

void loop() {
  // nulla qui
}
```

Output di esempio:
```
<!DOCTYPE html>
<html>
  <head>
    <title>Esempio Pagina</title>
  </head>
  <body>
    <p>Ciao dal web!</p>
  </body>
</html>
```

## Approfondimento
Collegare Arduino a Internet è una conquista relativamente recente. Prima si basava su shield Ethernet, ma ora, con moduli come ESP8266 e Wi-Fi integrato su ESP32, è più facile. Alternativamente, si può usare il modulo Ethernet o GSM. Per quanto riguarda l'implementazione, si usano le librerie come WiFi.h e HTTPClient.h per semplificare la connessione e la richiesta HTTP.

## Vedi anche:
- Documentazione su HTTPClient per ESP32: https://docs.espressif.com/projects/esp-idf/en/latest/esp32/api-reference/protocols/http_client.html
- Il progetto ESP8266 Arduino core: https://github.com/esp8266/Arduino
- Tutorial su come usare Arduino con Ethernet Shield: https://www.arduino.cc/en/Guide/ArduinoEthernetShield
