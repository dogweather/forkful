---
title:                "Lettura di un file di testo"
html_title:           "Arduino: Lettura di un file di testo"
simple_title:         "Lettura di un file di testo"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/arduino/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Cosa & Perché?
Leggere un file di testo è il processo di accedere e leggere il contenuto di un file di testo in un dispositivo elettronico, come ad esempio un computer o una scheda Arduino. I programmatori spesso leggono i file di testo per ottenere informazioni utili, come dati o istruzioni, da utilizzare nei loro programmi.

## Come fare:
Apriamo un file di testo e vi inseriamo alcune parole, come "Ciao mondo!". Il codice seguente mostra come leggerlo usando la scheda Arduino:

```
void setup(){
  Serial.begin(9600); //imposta la velocità di trasmissione seriale
  while (!Serial) {
    ; //aspetta che la comunicazione seriale sia stabilita
  }
  //apriamo il file di testo in modalità lettura
  File file = SD.open("test.txt", FILE_READ);
  if (file) {
    //leggi il contenuto del file finché ci sono informazioni da leggere
    while (file.available()) {
      Serial.write(file.read()); //stampiamo il contenuto sul monitor seriale
    }
    file.close(); //chiudiamo il file
  }
  else {
    Serial.println("Errore nell'apertura del file"); //messaggio di errore se il file non viene aperto correttamente
  }
}

void loop(){

}
```

L'output sul monitor seriale sarà "Ciao mondo!".

## Approfondimento:
Lettura dei file di testo è un'attività comune nella programmazione. Prima dell'avvento dei computer, i file di testo erano spesso utilizzati per memorizzare informazioni, come elenchi di nomi e numeri. Oggi, i file di testo sono ancora molto utilizzati per salvare dati e istruzioni di programma. Ci sono anche altre funzioni disponibili per leggere i file di testo, come la funzione "fgets" in C++.

## Vedi anche:
- [Reading text files with Arduino](https://www.arduino.cc/en/Tutorial/Files)
- [Reading and writing files with Arduino](https://www.arduino.cc/en/Tutorial/FileReadWrite)
- [Arduino documentation](https://www.arduino.cc/reference/en/language/functions/files/fileopen/)