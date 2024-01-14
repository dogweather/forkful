---
title:                "Arduino: Scaricare una pagina web"
simple_title:         "Scaricare una pagina web"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/arduino/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Perché

Scaricare una pagina web può sembrare un'attività banale, ma in realtà può essere molto utile quando si programma un progetto con Arduino. Questo permette di ottenere dati in tempo reale da fonti esterne come sensori online o pagine web contenenti informazioni aggiornate.

## Come Fare

Per scaricare una pagina web con Arduino, è necessario utilizzare una connessione Internet e un modulo WiFi o Ethernet. Esistono diverse librerie disponibili online per semplificare questo processo, come ad esempio la libreria ESP8266WiFi per i moduli WiFi o la libreria Ethernet per i moduli Ethernet.

Il primo passo è connettere il modulo WiFi o Ethernet alla rete Internet. Una volta stabilita la connessione, è possibile utilizzare il metodo `client.connect()` per stabilire una connessione con il server contenente la pagina web che si desidera scaricare.

Una volta stabilita la connessione, è possibile utilizzare il metodo `client.print()` per inviare una richiesta HTTP al server specificando il metodo di richiesta (GET, POST, ecc.) e il percorso della pagina web. Ad esempio, se si desidera scaricare la pagina web http://www.example.com/page.html, la richiesta HTTP dovrebbe essere `client.print("GET /page.html HTTP/1.1\r\n")`.

Successivamente, è necessario leggere la risposta del server utilizzando il metodo `client.read()`. La risposta sarà un codice di stato HTTP seguito dai dati della pagina web richiesta. Per salvare i dati, è possibile utilizzare una variabile di tipo stringa e aggiungere ogni riga della risposta utilizzando il metodo `client.readStringUntil('\n')`. Una volta ottenuti tutti i dati, è possibile utilizzarli nel programma Arduino come si desidera.

Ecco un esempio di codice per scaricare e stampare il contenuto della pagina web http://www.example.com/page.html:

```
#include <ESP8266WiFi.h> //libreria per il modulo WiFi

WiFiClient client; //crea un'istanza del client WiFi

void setup() {
  Serial.begin(9600); //inizializza la comunicazione seriale
  WiFi.begin("nome_rete", "password_rete"); //connessione alla rete WiFi
  while (WiFi.status() != WL_CONNECTED) { //aspetta la connessione alla rete
    delay(500);
    Serial.println("Connesso!");
  }
}

void loop() {
  if (client.connect("www.example.com", 80)) { //connette al server
    client.print("GET /page.html HTTP/1.1\r\n"); //richiesta HTTP
    client.println("Host: www.example.com"); //specificare l'host del server
    client.println("Connection: close"); //chiude la connessione dopo la richiesta
    client.println(); //fine della richiesta
    while (client.available()) { //legge la risposta del server
      String line = client.readStringUntil('\n'); //legge ogni riga della risposta
      Serial.println(line); //stampa la riga ricevuta
    }
  }
  client.stop(); //chiude la connessione con il server
  delay(5000); //aspetta 5 secondi prima di fare una nuova richiesta
}
```

## Approfondimenti

Scaricare una pagina web può essere un'operazione più complessa di quanto descritto sopra. Ad esempio, ci possono essere problemi di connessione o limitazioni del server che possono causare errori nella ricezione dei dati. Inoltre, è possibile specificare parametri aggiuntivi nella richiesta HTTP per personalizzare il download, come l'utilizzo di un cookie o l'invio di dati tramite il metodo POST. Per ulteriori informazioni, si consiglia di consultare la documentazione delle librerie utilizzate.

## Vedi Anche

- [Libreria ESP8266WiFi per moduli WiFi](https://github.com/esp8266/Arduino/tree/master/libraries/ESP8266WiFi)
- [Libreria Ethernet per moduli Ethernet](https://www.arduino.cc/en/Reference/Ethernet)
- [Protocollo HTTP](https://www.w3.org/Protocols/)
- [Sito ufficiale di Arduino](https://www.arduino.cc/)