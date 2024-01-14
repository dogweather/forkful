---
title:                "Arduino: Lavorare con yaml"
simple_title:         "Lavorare con yaml"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/arduino/working-with-yaml.md"
---

{{< edit_this_page >}}

## Perché

Programmare con Arduino può essere divertente e gratificante, ma quando si tratta di gestire grandi quantità di dati, può diventare complicato. Questo è dove YAML entra in gioco, offrendo un modo semplice e leggibile per organizzare e archiviare i dati, rendendo il processo di programmazione con Arduino ancora più semplice.

## Come fare

Per utilizzare YAML con Arduino, è necessario prima di tutto scaricare il parser YAML dal sito ufficiale di Arduino e installarlo nella propria directory di lavoro. Una volta fatto ciò, è possibile iniziare a scrivere il codice. Vediamo un esempio di codice che mostra come leggere e scrivere i dati da un file YAML tramite Arduino:

```
#include <YAML.h>

void setup() {
  Serial.begin(9600);
  YAML.begin(); //inizializza il parser YAML

  //scrivi i dati nel file YAML
  YAML.createNode("nome_file");
  YAML.addNode("nome_file", "nome", "John");
  YAML.addNode("nome_file", "cognome", "Doe");
  YAML.save("nome_file.yml"); //salva il file
  delay(100);

  //leggi i dati dal file YAML
  String nome = YAML.getValue("nome_file", "nome");
  String cognome = YAML.getValue("nome_file", "cognome");

  //stampa i dati sulla console seriale
  Serial.println("Nome: " + nome);
  Serial.println("Cognome: " + cognome);
}

void loop() {
  //il programma rimane in loop
}
```

Una volta caricato il codice su Arduino e aperta la console seriale, si dovrebbe vedere l'output seguente:

```
Nome: John
Cognome: Doe
```

Come si può vedere, utilizzare YAML con Arduino è piuttosto semplice e permette di organizzare facilmente i dati in modo leggibile.

## Approfondimento

Oltre all'utilizzo di base mostrato sopra, esistono altre funzioni che possono essere utili quando si lavora con YAML e Arduino. Una di queste è la possibilità di eliminare un nodo o un'intera struttura dati dal file YAML: ```YAML.deleteNode("nome_file", "nome")``` o ```YAML.deleteFile("nome_file.yml")```. È inoltre possibile creare una struttura dati annidata all'interno di un file YAML, permettendo una maggiore organizzazione dei dati.

Per ulteriori informazioni e opzioni avanzate, si consiglia di consultare la documentazione ufficiale di YAML per Arduino.

## Vedi anche

- [Sito ufficiale di Arduino](https://www.arduino.cc/)
- [Parser YAML per Arduino](https://arduino-yaml-3oyc91m93.vercel.app/)