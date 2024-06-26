---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:32:21.728359-07:00
description: "Come fare: Arduino non differenzia nativamente tra output standard e\
  \ errore standard come fanno i sistemi informatici convenzionali. Entrambi i metodi\u2026"
lastmod: '2024-03-13T22:44:43.698959-06:00'
model: gpt-4-0125-preview
summary: Arduino non differenzia nativamente tra output standard e errore standard
  come fanno i sistemi informatici convenzionali.
title: Scrivere sull'errore standard
weight: 25
---

## Come fare:
Arduino non differenzia nativamente tra output standard e errore standard come fanno i sistemi informatici convenzionali. Entrambi i metodi `Serial.print()` e `Serial.println()` scrivono sullo stesso output seriale, tipicamente visualizzato nel Monitor Seriale dell'IDE Arduino. Tuttavia, possiamo emulare la scrittura su stderr formattando specificamente i messaggi di errore o indirizzandoli a un output alternativo, come un file su una scheda SD o tramite una connessione di rete.

Per emulare stderr, puoi prefissare i messaggi di errore con un tag come "ERROR:" per differenziarli nel Monitor Seriale:

```cpp
void setup() {
  Serial.begin(9600); // Inizializza la comunicazione seriale a 9600 baud
}

void loop() {
  int result = someFunction();
  if (result == -1) {
    // Emulazione di stderr prefissando il messaggio di errore
    Serial.println("ERROR: La funzione ha fallito l'esecuzione.");
  } else {
    Serial.println("La funzione è stata eseguita con successo.");
  }
  delay(1000); // Attesa di un secondo prima di riavviare il ciclo
}

int someFunction() {
  // Una funzione fittizia che restituisce -1 in caso di errore
  return -1;
}
```

Un esempio di output nel Monitor Seriale dell'IDE Arduino potrebbe essere così:

```
ERROR: La funzione ha fallito l'esecuzione.
```

Per progetti che richiedono un approccio più sofisticato, inclusa la scrittura su output fisici diversi, può essere necessario l'uso di librerie di terze parti o hardware aggiuntivo. Ad esempio, per registrare i messaggi di errore su una scheda SD è necessaria la libreria `SD`:

```cpp
#include <SPI.h>
#include <SD.h>

File myFile;

void setup() {
  Serial.begin(9600);
  if (!SD.begin()) {
    Serial.println("ERROR: Inizializzazione della scheda SD fallita!");
    return;
  }
  
  myFile = SD.open("error.log", FILE_WRITE);
  if (myFile) {
    myFile.println("ERROR: La funzione ha fallito l'esecuzione.");
    myFile.close(); // Assicurati di chiudere il file per salvare i contenuti
  } else {
    Serial.println("ERROR: Apertura di error.log fallita!");
  }
}

void loop() {
  // Qui andrebbe il codice principale
}
```

Con questo approccio, separi fisicamente l'output normale del programma e i messaggi di errore indirizzando questi ultimi a un file `error.log` su una scheda SD, consentendo analisi post-mortem senza ingombrare il canale di output principale.
