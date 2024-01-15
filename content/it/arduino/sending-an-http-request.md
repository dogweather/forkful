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

## Perché
In questo articolo scoprirai come utilizzare la tua scheda Arduino per inviare richieste HTTP. Questo può essere utile per comunicare con altri dispositivi o server remoti, ad esempio per controllare un dispositivo a distanza o ricevere dati.

## Come fare
Per poter inviare una richiesta HTTP con Arduino, è necessario utilizzare un modulo WiFi o Ethernet. In questo esempio, utilizzeremo il modulo WiFi ESP8266 e una libreria apposita. Assicurati di avere già installato l'IDE di Arduino e di aver configurato correttamente il tuo modulo WiFi.

Una volta pronti, segui questi passaggi:

1. Includi la libreria "ESP8266WiFi.h" nel tuo sketch Arduino.
```
#include <ESP8266WiFi.h>
```

2. Definisci il tuo SSID e la password della tua rete WiFi.
```
const char* ssid = "NOME_RETE_WIFI";
const char* password = "PASSWORD_RETE_WIFI";
```

3. Inizializza la connessione WiFi nel metodo `setup()`:
```
void setup() {
    Serial.begin(9600);
    WiFi.begin(ssid, password);
    while(WiFi.status() != WL_CONNECTED) {
        delay(500);
        Serial.print(".");
    }
    Serial.println("Connesso alla rete WiFi");
}
```

4. Nel metodo `loop()`, crea una variabile `WiFiClient` e utilizzala per aprire una connessione TCP all'indirizzo del server e alla porta desiderati:
```
WiFiClient client;
if(client.connect("INDIRIZZO_SERVER", PORTA)) {
    Serial.println("Connesso al server");
    // Qui puoi scrivere la tua richiesta HTTP
}
```

5. Utilizza il metodo `client.println()` per scrivere la tua richiesta HTTP. Puoi anche utilizzare il metodo `client.print()` se vuoi inviare solo una parte della richiesta alla volta.
```
client.println("GET /pagina.html HTTP/1.1");
client.println("Host: INDIRIZZO_SERVER");
client.println("Connection: close");
client.println();
```

6. Leggi la risposta del server utilizzando il metodo `client.readStringUntil()` e stampala sulla seriale:
```
String response = client.readStringUntil('\r');
Serial.println(response);
```

7. Chiudi la connessione quando hai terminato di leggere la risposta:
```
client.stop();
```

## Approfondimento
Per capire meglio il processo di invio di una richiesta HTTP con Arduino, è importante conoscere le diverse parti che compongono una richiesta HTTP. Queste sono:

- il metodo della richiesta (GET, POST, PUT, ecc.)
- il percorso della risorsa (es. /pagina.html)
- l'Host (l'indirizzo del server a cui si sta inviando la richiesta)
- l'header della connessione (per specificare informazioni aggiuntive sulla richiesta)

Inoltre, è possibile inviare dati aggiuntivi tramite il corpo della richiesta, utilizzando ad esempio il metodo `client.print()`. Questo può essere utile per inviare informazioni al server o aggiungere parametri alla richiesta.

## Vedi anche
- [Libreria ESP8266WiFi](https://github.com/esp8266/Arduino/blob/master/libraries/ESP8266WiFi/src/ESP8266WiFi.h)
- [Tutorial su inviare richieste HTTP con Arduino](https://randomnerdtutorials.com/esp8266-http-get-post-arduino/)