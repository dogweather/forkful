---
title:                "Arduino: L'analisi di HTML"
simple_title:         "L'analisi di HTML"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/arduino/parsing-html.md"
---

{{< edit_this_page >}}

[Immagine di un Arduino UNO con dei fili elettrici collegati ad esso]

## Perché

Se sei un appassionato di Arduino e di programmazione, sicuramente avrai sentito parlare della tecnica di parsing HTML. Ma perché dovresti impegnarti in questa attività? Beh, in primo luogo, il parsing HTML ti consente di estrarre informazioni utili dai siti web, che possono essere utilizzate per realizzare progetti interessanti. In secondo luogo, può essere un modo divertente per mettere alla prova le tue capacità di programmazione e per acquisire nuove conoscenze.

## Come fare

Per iniziare, avrai bisogno di un Arduino board, dei cavi elettrici e di un computer con il software di programmazione Arduino IDE installato. Una volta che hai tutto il materiale, puoi seguire i seguenti passaggi:

1. Collega il tuo Arduino alla porta USB del computer.
2. Apri il software di programmazione e crea un nuovo sketch.
3. Utilizzando la libreria HTTPClient, effettua una richiesta GET ad un sito web.
4. Leggi il contenuto della risposta e scrivi un codice per estrarre le informazioni interessanti utilizzando la tecnica di parsing HTML.
5. Alla fine, puoi inviare i dati raccolti ad un display, un servomotore o qualsiasi altro componente che desideri utilizzare.

Di seguito, puoi trovare un esempio di codice che effettua una richiesta GET al sito "www.esempio.com" e stampa sul serial monitor il contenuto della pagina.

```Arduino
#include <HTTPClient.h>

void setup() {
  Serial.begin(9600); //Inizializza la comunicazione seriale
}

void loop() {
  if(WiFi.status()== WL_CONNECTED){  //Controlla la connessione WiFi
    HTTPClient http;    //Crea un oggetto HTTPClient
    http.begin("http://www.example.com");  //Specifica l'URL da cui effettuare la richiesta
    int httpCode = http.GET();   //Invia la richiesta GET
    if(httpCode > 0) {
      String content = http.getString();  //Salva il contenuto della risposta
      Serial.println(content);   //Stampa il contenuto sul serial monitor
    }
    else {
      Serial.println("Errore nella richiesta");  //Stampa un messaggio di errore
    }
    http.end();   //Termina la connessione
  }
  else {
    Serial.println("Connessione WiFi non disponibile");  //Stampa un messaggio se la connessione WiFi non è disponibile
  }
}
```

## Approfondimento

Il parsing HTML è fondamentale per estrarre informazioni dai siti web, ma può essere un'attività complessa. Ad esempio, potrebbe essere necessario considerare le diverse classi e ID degli elementi HTML per ottenere i dati desiderati. Inoltre, i siti web possono aggiornare il loro codice in qualsiasi momento, quindi il tuo codice di parsing potrebbe non funzionare più dopo un po' di tempo.

Per semplificare il processo, puoi utilizzare una libreria di parsing HTML come ad esempio "ArduinoWebParser" disponibile su GitHub. Questa libreria gestisce in modo più efficiente le varie etichette e attributi degli elementi HTML, semplificando così il tuo codice.

Un'altra opzione è quella di utilizzare un servizio di web scraping, che ti permette di estrarre facilmente i dati dai siti web senza dover scrivere alcun codice di parsing. Puoi trovare diverse opzioni online, come ad esempio "ParseHub" o "Octoparse".

## Vedi anche

- ArduinoWebParser: https://github.com/smerkousdavid/ArduinoWebParser
- ParseHub: https://www.parsehub.com/
- Octoparse: https://www.octoparse.com/it/