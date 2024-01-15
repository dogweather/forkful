---
title:                "Verifica se una directory esiste."
html_title:           "Arduino: Verifica se una directory esiste."
simple_title:         "Verifica se una directory esiste."
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/arduino/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Perché

Spesso, nei nostri progetti Arduino, è necessario verificare se una determinata directory esiste prima di procedere con l'esecuzione del codice. Ad esempio, potremmo voler salvare o recuperare file dalla scheda microSD e per farlo abbiamo bisogno di assicurarci che la directory in cui vogliamo lavorare esista.

## Come fare

Per verificare se una directory esiste, possiamo utilizzare la funzione `exists()` della libreria `SD`. Questo metodo restituirà `true` se la directory esiste e `false` in caso contrario. Vediamo un esempio di codice:

```
#include <SD.h> // includiamo la libreria SD nel nostro codice

// impostiamo il pin della scheda SD
const int chipSelect = 4;

void setup() {
  // inizializziamo la comunicazione seriale per il debug
  Serial.begin(9600);
  
  // inizializziamo la scheda SD
  if (!SD.begin(chipSelect)) {
    // nel caso in cui la scheda SD non sia stata inizializzata correttamente, 
    // stampiamo un messaggio di errore e non continuiamo l'esecuzione del programma
    Serial.println("Errore durante l'inizializzazione della scheda SD");
    return;
  }
  
  // definiamo il percorso della directory che vogliamo controllare
  File directory = SD.open("/nome_directory");
  
  // utilizziamo la funzione exists() per verificare se la directory esiste
  if (directory.exists()) {
    Serial.println("La directory esiste!");
  } else {
    Serial.println("La directory non esiste.");
  }
  
  // ricordiamoci sempre di chiudere i file dopo averli aperti
  directory.close();
}

void loop() {
  // il loop può essere lasciato vuoto per questo esempio
}
```

Supponendo di avere una scheda SD con una directory chiamata "nome_directory", il programma stampa il messaggio "La directory esiste!" sulla porta seriale. Se, invece, la directory non esiste, viene stampato il messaggio "La directory non esiste.".

## Approfondimento

La funzione `exists()` della libreria `SD` utilizza il metodo `filePath` della classe `File` per verificare l'esistenza della directory. Questo metodo restituisce una stringa vuota se la directory non esiste e il percorso completo della directory se questa esiste.

Per ulteriori informazioni su questa funzione e sulla classe `File`, puoi consultare la [documentazione ufficiale di Arduino](https://www.arduino.cc/reference/en/libraries/sd/filesys-h/exists/).

## Vedi anche

- [Tutorial: Come utilizzare una scheda SD con Arduino](https://www.instructables.com/id/How-to-use-SD-card-with-Arduino/)
- [Libreria SD di Arduino](https://www.arduino.cc/en/reference/SD)
- [Class File di Arduino](https://www.arduino.cc/en/Reference/File)