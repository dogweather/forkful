---
title:                "Inviare una richiesta http"
html_title:           "Arduino: Inviare una richiesta http"
simple_title:         "Inviare una richiesta http"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/arduino/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Che cos'è e perché?

In Arduino, il termine "HTTP request" si riferisce all'azione di inviare una richiesta ad un server web per ottenere informazioni o eseguire un'azione. I programmatori spesso utilizzano le HTTP request per comunicare con altri servizi o per ottenere informazioni da fonti esterne.

## Come fare:

Ecco un esempio di codice per inviare una HTTP request in Arduino:

```Arduino
#include <WiFiClient.h>
int httpPort = 80; // specificare la porta del server web
char server[] = "www.example.com"; // specificare l'indirizzo del server

void setup() {
  Serial.begin(9600); // inizializza la comunicazione seriale
  WiFi.begin("", ""); // connettersi alla rete WiFi
  while (WiFi.status() != WL_CONNECTED) { // attendi la connessione WiFi
    delay(500);
  }
  Serial.println("Connesso alla rete WiFi!");
}

void loop() {
  WiFiClient client; // crea un nuovo client WiFi
  Serial.print("Connettendosi al server: ");
  Serial.println(server);

  if (client.connect(server, httpPort)) { // connetti il client al server
    Serial.println("Connessione riuscita!");
    client.println("GET / HTTP/1.1"); // invia la richiesta al server
    client.println(); // fine della richiesta
  } else {
    Serial.println("Connessione fallita.");
  }

  while (client.connected()) { // leggi la risposta del server
    if (client.available()) {
      char c = client.read();
      Serial.print(c);
    }
  }

  Serial.println("Fine della risposta del server.");
  client.stop(); // chiudi la connessione
  delay(30000); // attendi 30 secondi prima di inviare un'altra richiesta
}
```

Esempio di output:
```
Connesso alla rete WiFi!
Connettendosi al server: www.example.com
Connessione riuscita!
HTTP/1.1 200 OK
Server: Apache
Content-Type: text/html; charset=UTF-8
Date: Mon, 12 Apr 2021 00:00:00 GMT
Connection: close
Content-Length: 50

<html><body><h1>Benvenuti su www.example.com!</h1></body></html>
Fine della risposta del server.
```

## Approfondimenti:

Le HTTP request sono un'importante componente della comunicazione web e sono state introdotte nel 1991 da Tim Berners-Lee, il creatore del World Wide Web. Oltre alla GET request mostrata sopra, ci sono altre tipi di richiesta come POST, PUT, DELETE che consentono di eseguire diverse azioni sui servizi web. Un'alternativa alla libreria Wi-FiClient di Arduino è l'utilizzo di cURL, un popolare strumento di linea di comando per effettuare HTTP request.

## Vedi anche:

- Documentazione di Arduino per le HTTP request: https://www.arduino.cc/en/Reference/HTTPClient
- Maggiori informazioni sulle HTTP request e i diversi tipi: https://developer.mozilla.org/en-US/docs/Web/HTTP/Methods