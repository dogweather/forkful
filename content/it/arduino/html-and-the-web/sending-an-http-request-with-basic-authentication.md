---
date: 2024-01-20 18:00:58.452125-07:00
description: "Come Fare: L'autenticazione HTTP di base esiste da quando l'HTTP \xE8\
  \ stato creato nei primi anni '90. \xC8 un metodo semplice ma meno sicuro rispetto\
  \ a metodi\u2026"
lastmod: '2024-04-05T21:53:44.444053-06:00'
model: gpt-4-1106-preview
summary: "L'autenticazione HTTP di base esiste da quando l'HTTP \xE8 stato creato\
  \ nei primi anni '90."
title: Inviare una richiesta http con autenticazione di base
weight: 45
---

## Come Fare:
```Arduino
#include <ESP8266WiFi.h>
#include <Base64.h>

const char* ssid = "yourSSID"; 
const char* password = "yourPASSWORD";

const char* host = "www.esempio.com";
const int httpPort = 80;

void setup() {
  Serial.begin(115200);
  WiFi.begin(ssid, password);

  while (WiFi.status() != WL_CONNECTED) {
    delay(500);
    Serial.print(".");
  }
  
  Serial.println("Connesso al WiFi");

  // Autenticazione base codificata in Base64
  String auth = "username:password";
  String authEncoded = base64::encode(auth);
  
  // Connessione al server
  WiFiClient client;
  if (!client.connect(host, httpPort)) {
    Serial.println("Connessione fallita");
    return;
  }
  
  // Invio della richiesta HTTP con header di autenticazione
  client.print(String("GET ") + "/percorso/risorsa" + " HTTP/1.1\r\n" +
               "Host: " + host + "\r\n" +
               "Authorization: Basic " + authEncoded + "\r\n" +
               "Connection: close\r\n\r\n");
               
  Serial.println("Richiesta HTTP inviata");

  // Attendi la risposta del server e stampala
  while (client.connected()) {
    String line = client.readStringUntil('\n');
    if (line == "\r") {
      break; // Fine degli headers
    }
  }
  
  // Stampa la riposta del server
  String line = client.readStringUntil('\n');
  Serial.println(line);
}

void loop() {
  // Qui si può inserire del codice per inviare periodicamente nuove richieste
}
```

## Approfondimenti:
L'autenticazione HTTP di base esiste da quando l'HTTP è stato creato nei primi anni '90. È un metodo semplice ma meno sicuro rispetto a metodi più moderni come OAuth, poiché le credenziali sono codificate in Base64 e non crittografate, rendendole vulnerabili ad intercettazioni. Tuttavia, è ancora usata in contesti meno critici o in reti chiuse dove la sicurezza non è una grande preoccupazione. Un'alternativa potrebbe essere l'uso di chiavi API o tokens. A livello implementativo, è importante che l'Arduino sia connesso a una rete affidabile e che la velocità seriale (baud rate) coincida con quella impostata nell'IDE Arduino per una corretta comunicazione.

## Vedi Anche:
- Documentazione ufficiale Arduino su WiFi: https://www.arduino.cc/en/Reference/WiFi
- Documentazione su HTTP Basic Authentication (RFC7617): https://tools.ietf.org/html/rfc7617
- Libreria Base64 per Arduino: https://www.arduino.cc/reference/en/libraries/base64/
