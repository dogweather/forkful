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

## Perché

Quando si utilizza una scheda Arduino per comunicare con il web, può essere necessario inviare richieste HTTP con l'autenticazione di base. Questo è particolarmente utile se si vuole accedere a risorse protette da password.

## Come fare

Per inviare una richiesta HTTP con autenticazione di base utilizzando Arduino, è necessario utilizzare la libreria ESP8266WiFi (se si sta utilizzando un modulo WiFi) o la libreria Ethernet (se si sta utilizzando una scheda Ethernet). In entrambi i casi, si deve impostare la comunicazione con il server web utilizzando la classe WiFiClient (per ESP8266) o la classe EthernetClient (per Ethernet).

```Arduino
#include <WiFiClient.h>   // o #include <EthernetClient.h> se si utilizza Ethernet

// Creare l'oggetto cliente
WiFiClient client;   // o EthernetClient client; se si utilizza Ethernet

// Connessione al server web
if (client.connect(server, port)) {   // impostare server e porta del server web desiderato
  // Generare una stringa di autenticazione di base
  String authString = "Basic " + base64Encode(username + ":" + password);   // sostituire username e password con le proprie credenziali

  // Invio della richiesta con l'intestazione di autenticazione di base
  client.println("GET /protected-resource HTTP/1.1");   // sostituire "/protected-resource" con il percorso della risorsa desiderata
  client.println("Host: www.example.com");   // sostituire "www.example.com" con l'indirizzo del server web
  client.println("Authorization: " + authString);   // attenzione alle virgolette: la stringa deve iniziare con "Basic "
  client.println();   // invia una riga vuota per indicare la fine dell'intestazione
}

// Lettura della risposta
while (client.available()) {
  String response = client.readStringUntil('\r');   // legge la risposta del server fino alla fine della riga

  // Fare qualcosa con la risposta ottenuta
  Serial.println(response);   // stampa la risposta sulla seriale
}

// Disconnessione dal server web
client.stop();
```

Si noti che prima di inviare la richiesta, è necessario generare una stringa di autenticazione base64 utilizzando le proprie credenziali e inserirla nell'intestazione della richiesta. Inoltre, è importante controllare se la connessione al server è stata stabilita correttamente prima di inviare la richiesta.

È possibile utilizzare questo stesso metodo per inviare richieste HTTP con altri metodi (ad esempio POST o PUT), basta modificare la prima riga della richiesta (GET nel codice sopra) in base al metodo desiderato.

## Approfondimenti

L'autenticazione di base è solo uno dei vari modi per effettuare l'autenticazione in una richiesta HTTP. Altri metodi comuni includono OAuth e Token-based authentication. È importante utilizzare l'autenticazione adeguata in base alle esigenze specifiche del proprio progetto.

Un'altra cosa da tenere a mente è che l'autenticazione di base non è sicura in quanto le credenziali vengono inviate in chiaro. È consigliato utilizzare una connessione HTTPS per garantire la sicurezza delle credenziali.

## Vedi anche

[Documentazione ufficiale di Arduino](https://www.arduino.cc/en/Reference/ClientConstructor)

[Tutorial su HTTP Requests con Arduino](https://lastminuteengineers.com/esp8266-http-get-post-arduino/)