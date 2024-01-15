---
title:                "Scaricando una pagina web"
html_title:           "Arduino: Scaricando una pagina web"
simple_title:         "Scaricando una pagina web"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/arduino/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Perché
Se vuoi integrare la tua scheda Arduino con il mondo online e accedere ai dati di Internet, è fondamentale saper scaricare e analizzare una pagina web. In questo modo potrai creare progetti interattivi e ottenere informazioni direttamente dal web.

## Come fare
Per iniziare a scaricare una pagina web, avrai bisogno di alcune librerie. Per fortuna, il compilatore di Arduino ha una vasta gamma di librerie tra cui scegliere.

```Arduino
#include <WiFi.h>
#include <WiFiClient.h>
#include <WiFiMulti.h>
#include <HTTPClient.h>
```

Una volta inclusi questi componenti nel tuo codice, potrai utilizzare l'oggetto `HTTPClient` per connetterti al server web e scaricare una pagina. Di seguito un esempio di codice che scarica la pagina in formato testo e la stampa sulla console seriale.

```Arduino
WiFiClient client;
HTTPClient http;

// Connetti WiFi
WiFi.begin(ssid, password);
while (WiFi.status() != WL_CONNECTED) {
  delay(500);
  Serial.println("Sto cercando la rete WiFi...");
}
Serial.println("Connesso alla rete WiFi!");

// Connessione al server e scarico della pagina
Serial.print("Connetto a ");
Serial.println(webserver);

if (http.begin(client, webserver)) {
  int httpCode = http.GET();
  if (httpCode > 0) {
    String payload = http.getString();
    Serial.println(payload);
  }
  http.end();
} else {
  Serial.println("Non posso connettermi al server");
}
```

Il codice è abbastanza semplice ma molto efficace. Ti consiglio di approfondire con la documentazione delle librerie incluse per personalizzare la tua connessione e i dati da scaricare.

## Approfondimento
Scaricare una pagina web è solo l'inizio di ciò che puoi fare con l'Arduino e il mondo online. Utilizzando le librerie corrette, potrai anche analizzare il contenuto delle pagine scaricate, reagire in base a determinate informazioni o addirittura controllare il tuo Arduino da remoto tramite il web. Non ci sono limiti alle possibilità!

## Vedi Anche
- [Documentazione ufficiale di Arduino](https://www.arduino.cc/reference/en/)
- [Esempi delle librerie WiFi e HTTPClient](https://github.com/arduino-libraries/WiFi/tree/master/examples)
- [Tutorial su come utilizzare l'Arduino con il web](https://create.arduino.cc/projecthub/Matrix-Rex/connect-arduino-wifi-module-with-internet-using-at-commands-4cf6f0)