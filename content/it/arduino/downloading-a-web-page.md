---
title:                "Scaricare una pagina web"
html_title:           "C++: Scaricare una pagina web"
simple_title:         "Scaricare una pagina web"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/arduino/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Che cosa & Perché?

Scaricare una pagina web significa prelevare il codice di programmazione di un sito web direttamente sul tuo dispositivo. I programmatori lo fanno per estrarre dati, eseguire analisi o salvare una copia locale del sito.

## Come fare:

Per scaricare una pagina web con Arduino, usa la libreria Ethernet. Ecco un esempio di codice:

```Arduino
#include <Ethernet.h>

byte mac[] = {0xDE, 0xAD, 0xBE, 0xEF, 0xFE, 0xED};
char server[] = "www.tuosito.it"; 

EthernetClient client;

void setup() {
  Ethernet.begin(mac);
  delay(1000);
  
  if(client.connect(server, 80)) {
    client.println("GET / HTTP/1.1");
    client.println("Host: www.tuosito.it");
    client.println("Connection: close");
    client.println();
  }
}

void loop() {
  if(client.available()) {
    char c = client.read();
    Serial.print(c);
  }

  if(!client.connected()) {
    client.stop();
  }
}
```

Dopo aver caricato questo sketch, l'output del monitor seriale dovrebbe mostrare il codice HTML della pagina web.

## Approfondimento

Storicamente, avevamo bisogno di computer potenti per scaricare pagine web. Ma adesso, con i microcontrollori come Arduino, possiamo farlo facilmente. Un'altra libreria per fare la stessa cosa è WiFi101. In termini di implementazione, ricorda che non tutti i siti consentono web scraping, quindi verifica le politiche del sito prima di scaricare.

## Vedi anche

Per approfondire, dai un'occhiata a queste risorse:

1. Documentazione ufficiale dell'Arduino su Ethernet - [https://www.arduino.cc/en/Reference/Ethernet](https://www.arduino.cc/en/Reference/Ethernet)
2. Tutorial su come utilizzare la libreria WiFi101 - [https://www.arduino.cc/en/Guide/WiFi101](https://www.arduino.cc/en/Guide/WiFi101) 
3. Una guida al web scraping etico - [http://webscraping.com/data-extraction-guide](http://webscraping.com/data-extraction-guide)