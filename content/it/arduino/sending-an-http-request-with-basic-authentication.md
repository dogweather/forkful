---
title:                "Arduino: Inviare una richiesta http con autenticazione di base"
simple_title:         "Inviare una richiesta http con autenticazione di base"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/arduino/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Perché

Molte volte, quando si utilizza Arduino per progetti di automazione o di monitoraggio, si desidera che il nostro dispositivo sia in grado di comunicare con un server o un'API. In questi casi, potrebbe essere necessario inviare una richiesta HTTP con autenticazione di base per accedere ai dati o eseguire determinate operazioni. Vediamo come farlo utilizzando Arduino.

## Come fare

Per inviare una richiesta HTTP con autenticazione di base, è necessario seguire alcuni passaggi fondamentali. Prima di tutto, dobbiamo includere la libreria "WiFi.h" e la libreria "HTTPClient.h" nel nostro codice. Inoltre, dobbiamo definire le nostre credenziali di autenticazione nella sezione "setup", utilizzando il metodo "setAuthorization" della libreria HTTPClient.

Una volta fatto ciò, dobbiamo stabilire la connessione WiFi con la nostra rete e utilizzare il metodo "begin" della libreria HTTPClient per iniziare la richiesta. Inseriamo l'URL desiderato e il metodo HTTP che vogliamo utilizzare (GET, POST, PUT, DELETE). Se la richiesta viene eseguita correttamente, possiamo visualizzarne l'output con il metodo "readString".

Ecco un esempio di codice che esegue una richiesta GET con autenticazione di base a un'API:

```Arduino
#include <WiFi.h>
#include <HTTPClient.h>

void setup()
{
  Serial.begin(9600);
  
  WiFi.begin("nome_rete", "password_rete"); // sostituire con i tuoi dati di rete
  
  while (WiFi.status() != WL_CONNECTED)
  {
    delay(1000);
    Serial.println("Connessione WiFi in corso...");
  }
  Serial.println("Connesso alla rete WiFi!");
}

void loop()
{
  HTTPClient http;
  http.setAuthorization("username", "password"); // sostituire con le tue credenziali di autenticazione
  http.begin("URL_desiderato");
  int httpCode = http.GET();
  
  if (httpCode > 0)
  {
    String response = http.getString();
    Serial.println(response);
  }
  
  http.end();
  delay(5000); // attendiamo 5 secondi prima di eseguire una nuova richiesta
}
```

L'output della richiesta verrà visualizzato sulla console seriale di Arduino. Ovviamente, è possibile personalizzare il codice per adattarlo alle nostre esigenze specifiche.

## Approfondimento

Mentre il codice presentato sopra funziona bene per la maggior parte delle richieste HTTP, ci sono alcune situazioni in cui potrebbe essere necessario gestire manualmente alcune parti della richiesta. Ad esempio, se la risposta dell'API contiene dei cookie, dobbiamo essere in grado di leggerli e salvarli per eventuali richieste future.

Per fare ciò, possiamo utilizzare il metodo "getHeaders" della libreria HTTPClient per ottenere tutti gli header della risposta e il metodo "find" per individuare i cookie specifici che ci interessano.

Inoltre, se la nostra richiesta deve inviare dei parametri, dobbiamo utilizzare il metodo "addHeader" per aggiungere l'header "Content-Type" e il metodo "addParameter" per aggiungere i parametri necessari.

Per ulteriori informazioni sulla gestione avanzata delle richieste HTTP con Arduino, è possibile consultare la documentazione ufficiale delle librerie WiFi e HTTPClient.

## Vedi anche

Per saperne di più su come utilizzare Arduino per comunicare con il web, puoi leggere questi articoli:

- [Tutorial: Utilizzare Arduino per inviare email](https://www.circuitlab.it/arduino-invio-email/)
- [Come leggere dati da un server web con Arduino](https://www.circuitlab.it/arduino-leggere-dati-web/)
- [Guida all'utilizzo delle librerie WiFi e HTTPClient su Arduino](https://www.arduino.cc/en/Reference/WiFi)
- [Documentazione ufficiale delle librerie WiFi e HTTPClient](https://arduino-httpclient.readthedocs.io/en/latest/)