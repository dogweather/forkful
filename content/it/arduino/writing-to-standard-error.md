---
title:                "Scrivere su errore standard"
html_title:           "Arduino: Scrivere su errore standard"
simple_title:         "Scrivere su errore standard"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/arduino/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Cosa & Perché?
Scrivere sull'errore standard è una tecnica comunemente usata dai programmatori per visualizzare gli errori o le informazioni di debug durante l'esecuzione del codice. Questo metodo fornisce una rapida e facile visualizzazione dei messaggi senza interrompere il flusso del programma.

## Come fare:
Ecco un esempio semplice per scrivere su stderr utilizzando la libreria Arduino SoftwareSerial:

```Arduino
#include <SoftwareSerial.h>

SoftwareSerial mySerial(10, 11); // RX, TX

void setup() {
  Serial.begin(9600); // inizializza la porta seriale predefinita
  mySerial.begin(9600); // inizializza la porta seriale su pin 10 e 11
}

void loop() {
  mySerial.println("Questo è un messaggio di debug"); // scrive il messaggio su stderr
}
```
L'esempio illustra come è possibile utilizzare la classe SoftwareSerial per impostare una porta seriale su pin diversi da quelli predefiniti di Arduino. Utilizzando la funzione `println()` è possibile scrivere un messaggio su stderr e visualizzarlo tramite un monitor seriale come il "Monitor seriale" nell'IDE di Arduino.

## Approfondimento:
Scrivere su stderr è un'abilità fondamentale per i programmatori, poiché consente di identificare facilmente gli errori e le informazioni di debug durante lo sviluppo dei programmi. Questa tecnica è stata introdotta per la prima volta nel linguaggio di programmazione C, che è stato il predecessore del linguaggio di Arduino. Alcune alternative a stderr includono l'utilizzo di LED o display LCD per visualizzare le informazioni di debug.

Per implementare la scrittura su stderr su Arduino, è possibile utilizzare diverse librerie come SoftwareSerial, come mostrato nell'esempio sopra, o la libreria standard `Serial` di Arduino. Inoltre, è importante sapere che la libreria Arduino scrive automaticamente su stderr gli errori di compilazione, quindi non è necessario aggiungere alcun codice aggiuntivo per questi tipi di errori.

## Vedi anche:
Per ulteriori informazioni sulla scrittura su stderr e sulle alternative, puoi consultare la documentazione ufficiale di Arduino o la community di sviluppatori. Inoltre, puoi trovare molti tutorial e progetti che utilizzano questa tecnica su siti di programmazione come GitHub e StackOverflow.