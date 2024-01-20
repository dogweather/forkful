---
title:                "Inviare una richiesta http con autenticazione di base"
html_title:           "Arduino: Inviare una richiesta http con autenticazione di base"
simple_title:         "Inviare una richiesta http con autenticazione di base"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/arduino/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

# Invia una richiesta HTTP con autenticazione di base in Arduino

## Cos'è e Perché?
L'invio di una richiesta HTTP con autenticazione di base è un metodo per collegare il tuo sketch Arduino ad un servizio web protetto da password. Lo usiamo per accedere in modo sicuro ai dati o alle funzionalità che desideriamo.

## Come fare:
L'exempio seguente mostra come inviare una richiesta HTTP con autenticazione di base.


```Arduino
#include <ESP8266WiFi.h>
#include <ESP8266HTTPClient.h>

const char* ssid = "il_tuo_ssid";
const char* password = "la_tua_password";

void setup() {
  Serial.begin(115200);
  WiFi.begin(ssid, password);

  while (WiFi.status() != WL_CONNECTED) {
    delay(1000);
    Serial.println("Connettendo...");
  }
}

void loop() {

  if (WiFi.status() == WL_CONNECTED) {
    HTTPClient http;
    
    http.begin("http://il-tuo-sito-web.com");
    http.setAuthorization("il_tuo_utente", "la_tua_password");

    int httpCode = http.GET();
    
    if (httpCode > 0) {
      String payload = http.getString();
      Serial.println(payload);
    }

    http.end();
  }

  delay(30000); 
}
```
Nell'output, vedrai la risposta del server al tuo sketch Arduino.

## Approfondimento
L'autenticazione di base HTTP è un protocollo antico e diffuso, ma ricorda che trasmette le credenziali in chiaro (anche se codificate in Base64). Potresti prendere in considerazione l'uso di HTTPS per una maggiore sicurezza. Inoltre, esistono alternative più sicure all'autenticazione di base HTTP, come Digest Authentication o l'uso di token JWT.

Un dettaglio interessante riguardo a come inviare le richieste HTTP con autenticazione di base in Arduino: la funzione `setAuthorization` imposta l'header 'Authorization' della richiesta HTTP. È essenziale che le credenziali siano corrette, altrimenti il server restituirà un codice di stato '401 Non autorizzato'.

## Vedi Anche
- [HTTP Basic Authentication su Wikipedia](https://it.wikipedia.org/wiki/Basic_access_authentication)
- [Autenticazione Digest su Wikipedia](https://it.wikipedia.org/wiki/Digest_access_authentication)