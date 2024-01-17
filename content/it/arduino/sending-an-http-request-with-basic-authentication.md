---
title:                "Invio di una richiesta http con autenticazione di base"
html_title:           "Arduino: Invio di una richiesta http con autenticazione di base"
simple_title:         "Invio di una richiesta http con autenticazione di base"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/arduino/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Che cosa & Perché?
In questa guida parleremo di come inviare una richiesta HTTP con autenticazione di base utilizzando Arduino. Questa è una pratica comune tra i programmatori per accedere e ottenere dati da servizi web protetti.

## Come fare:
1) Per prima cosa, assicurati di avere una connessione internet stabile e funzionante per il tuo Arduino.
2) Quindi, definisci l'URL del servizio web al quale vuoi inviare la richiesta. Ad esempio:
```
char url[] = "https://www.example.com";
```
3) Ora, dovrai impostare le credenziali di autenticazione di base del servizio web. Questo di solito avviene attraverso un username e una password, che verranno codificati in base64 e inseriti nell'header della tua richiesta. Ad esempio:
```
char username[] = "utente";
char password[] = "pass123";
String auth = username + ":" + password;
char base64Auth[auth.length()];
auth.toCharArray(base64Auth, auth.length());
```
4) Successivamente, dovrai creare e configurare la tua richiesta HTTP. Puoi farlo utilizzando la libreria WiFiClient di Arduino e specificando il tipo di richiesta (GET, POST, ecc.), l'URL e l'header contenente le credenziali di autenticazione di base. Ad esempio:
```
WiFiClient client;
client.setTimeout(5000);
if (client.connect(url, 443)) { // 443 per HTTPS, 80 per HTTP
  client.println("GET /api/dati HTTP/1.1");
  client.print("Authorization: Basic ");
  client.println(base64Auth);
  client.println("Host: www.example.com");
  client.println("Connection: close");
  client.println();
}
```
5) Ora puoi inviare la tua richiesta e leggere la risposta del servizio web. Ad esempio, puoi utilizzare il metodo readStringUntil() per leggere i dati dall'API:
```
while (client.connected()) {
  String line = client.readStringUntil('\n');
  // Fare qualcosa con i dati letti
}
```

## Approfondimenti:
1) L'autenticazione di base è una delle tante metodologie di autenticazione utilizzate dai servizi web per proteggere i propri dati. Altre opzioni possono includere l'autenticazione con token o con chiave API.
2) Puoi utilizzare altri tipi di comunicazione come MQTT o WebSocket per inviare e ricevere dati dall'Arduino a un servizio web, ma l'autenticazione di base è ancora una pratica comune per accedere a dati protetti.
3) Per garantire la sicurezza nella comunicazione tra Arduino e servizio web, è consigliato utilizzare HTTPS invece di HTTP. Ciò significa che avrai bisogno di un certificato SSL installato sul tuo server per stabilire la connessione con Arduino.

## Vedi anche:
- [Libreria WiFiClient di Arduino](https://www.arduino.cc/en/Reference/WiFiClient)
- [Esempi di autenticazione di base con Arduino](https://randomnerdtutorials.com/send-get-requests-arduino/)
- [Documentazione sull'autenticazione di base](https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication)