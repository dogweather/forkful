---
title:                "Arduino: Inviare una richiesta http"
simple_title:         "Inviare una richiesta http"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/arduino/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Perché

Ci sono molte ragioni per cui potresti voler sperimentare con l'invio di richieste HTTP utilizzando Arduino. Forse vuoi accedere a dati da un server remoto per il tuo progetto, o vuoi creare una connessione Internet con un dispositivo OTA (over-the-air). In ogni caso, l'utilizzo di richieste HTTP con Arduino può aiutarti a realizzare progetti interessanti ed innovativi.

## Come Fare

Per inviare una richiesta HTTP utilizzando Arduino, avrai bisogno di alcuni componenti chiave: una scheda di sviluppo Arduino, un modulo WiFi o Ethernet per la connettività Internet e il codice corretto per eseguire la richiesta HTTP desiderata. In questo articolo, useremo un esempio semplice di come eseguire una richiesta GET verso un server web utilizzando il modulo WiFi ESP8266.

```
Arduino #include <ESP8266WiFi.h>

// sostituisci le seguenti informazioni con la tua rete WiFi e indirizzo IP del server
const char* ssid = "nome della tua rete WiFi";
const char* password = "password della tua rete WiFi";
const char* serverIP = "indirizzo IP del server";

void setup() {
  // inizializza la connessione WiFi
  WiFi.begin(ssid, password);

  // attendi che la connessione WiFi venga stabilita
  while (WiFi.status() != WL_CONNECTED) {
    delay(500);
  }
}

void loop() {
  // usa l'oggetto WiFiClient per stabilire una connessione con il server
  WiFiClient client;
  if (client.connect(serverIP, 80)) {
    // invia una richiesta GET per la pagina principale del server
    client.print("GET / HTTP/1.1\r\n");
    client.print("Host: ");
    client.print(serverIP);
    client.print("\r\n\r\n");

    // leggi la risposta del server e stampala sulla console seriale
    while (client.available()) {
      char c = client.read();
      Serial.print(c);
    }
  }
  // chiudi la connessione
  client.stop();

  // attendi per un determinato intervallo prima di ripetere la richiesta
  delay(5000);
}
```

L'output sul monitor seriale dovrebbe essere qualcosa di simile a questo:

```
Connecting to nome della tua rete WiFi
......
WiFi connected
HTTP/1.1 200 OK
Date: Mon, 06 Sep 2021 10:00:13 GMT
Content-Type: text/html
Connection: close
```

## Approfondimento

Oltre all'esempio fornito, ci sono diverse considerazioni importanti da tenere a mente quando si effettuano richieste HTTP con Arduino. Una di queste è la sicurezza dei dati: assicurati di utilizzare una connessione sicura (HTTPs) quando invii informazioni sensibili su una rete. Inoltre, assicurati di gestire gli errori durante la connessione con il server e durante la ricezione della risposta.

Vale anche la pena notare che inviare richieste HTTP utilizzando Arduino potrebbe non essere la soluzione migliore per progetti che richiedono connessioni internet complesse o trasferimento di grandi quantità di dati. In questi casi, potresti voler considerare l'utilizzo di una scheda di sviluppo più potente o di un microcontrollore più avanzato.

## Vedi Anche

- [Tutorial su ESP8266 e richieste HTTP](https://randomnerdtutorials.com/esp8266-nodemcu-post-request-data-to-web-server/)
- [Libreria WiFi ESP8266 per Arduino](https://arduino-esp8266.readthedocs.io/en/latest/index.html)
- [Guida alle richieste HTTP con Arduino](https://arduinojson.org/how-to/how-to-use-arduinojson-with-firebase/)
- [Scheda di sviluppo Arduino Uno](https://store.arduino.cc/arduino-uno-rev3)
- [Modulo WiFi ESP8266](https://www.sparkfun.com/products/13678)