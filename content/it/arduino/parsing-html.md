---
title:                "Analisi dell'HTML"
date:                  2024-01-20T15:29:53.293117-07:00
simple_title:         "Analisi dell'HTML"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/arduino/parsing-html.md"
---

{{< edit_this_page >}}

## Che Cosa & Perché?
Il parsing HTML consiste nell'analizzare il codice HTML per estrarre dati specifici. I programmatori lo fanno per interagire con il web, raccogliere informazioni o integrare funzionalità di terze parti nelle loro applicazioni.

## Come fare:
Ecco un esempio semplice che mostra come connettere un Arduino alla rete, fare una richiesta HTTP e effettuare il parsing di una risposta HTML.

```Arduino
#include <Ethernet.h>
#include <SPI.h>

// Inizializza la libreria Ethernet con l'indirizzo MAC e l'IP del tuo Arduino
byte mac[] = { 0xDE, 0xAD, 0xBE, 0xEF, 0xFE, 0xED };
IPAddress ip(192, 168, 1, 177);
EthernetClient client;

void setup() {
  Ethernet.begin(mac, ip);
  Serial.begin(9600);
  while (!Serial) {
    ; // aspetta la connessione della porta seriale
  }

  if (client.connect("example.com", 80)) {
    client.println("GET /pagina.html HTTP/1.1");
    client.println("Host: example.com");
    client.println("Connection: close");
    client.println();
  }
}

void loop() {
  while (client.connected()) {
    if (client.available()) {
      char c = client.read();
      // Qui puoi fare il parsing di c
      Serial.print(c);
    }
  }
  
  client.stop();
}
```

Risultato: Output della risposta HTML sul monitor seriale.

## Approfondimenti:
Il parsing HTML con Arduino è una versione molto semplice di ciò che si potrebbe fare con un computer completo. Storicamente, il parsing è stato fatto con linguaggi come Python o Java, ma con l'avvento di dispositivi IoT come Arduino, il parsing è diventato popolare anche in queste piattaforme.

Alternativamente, si può usare una libreria di parsing HTML come "ArduinoHtmlParser" che gestisce molti dettagli di parsing, ma questa aggiunge complessità e utilizza spazio in memoria. Va considerato che Arduino non ha le stesse capacità di elaborazione o la quantità di memoria di altri dispositivi, quindi il parsing HTML deve essere il più snello possibile.

## Vedi Anche:
- Documentazione ufficiale di Ethernet library per Arduino: https://www.arduino.cc/en/Reference/Ethernet
- Un'introduzione al parsing HTML: https://www.codeproject.com/Articles/298519/Fast-and-simple-HTML-parsing
- Libreria ArduinoHtmlParser: https://github.com/forcer/ArduinoHtmlParser
