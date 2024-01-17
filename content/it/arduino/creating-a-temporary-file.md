---
title:                "Creazione di un file temporaneo"
html_title:           "Arduino: Creazione di un file temporaneo"
simple_title:         "Creazione di un file temporaneo"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/arduino/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Cosa & perché?
 Creare un file temporaneo è un'operazione comune nella programmazione di siti web, software e dispositivi elettronici. Questo perché i file temporanei ci permettono di archiviare temporaneamente informazioni che non ci servono a lungo termine, senza dover occupare memoria permanente o spazio di archiviazione.

## Come:
In Arduino, puoi creare un file temporaneo utilizzando la funzione ```createTempFile()```. Di seguito puoi trovare un semplice esempio di come creare un file temporaneo e scrivere del testo al suo interno:

```Arduino
File tmpFile = createTempFile(); //crea il file temporaneo
tmpFile.println("Questo è un testo da scrivere nel file."); //scrive il testo nel file
tmpFile.close(); //chiude il file
```

Una volta eseguito il codice, puoi verificare la creazione del file e il suo contenuto tramite il monitor seriale.

## Approfondimento:
Creare un file temporaneo è una pratica comune nella programmazione da diversi anni. In passato, i file temporanei venivano creati manualmente, mentre oggi puoi utilizzare le funzioni apposite presenti in molte librerie di programmazione. In alternativa, puoi anche utilizzare variabili temporanee per archiviare temporaneamente dati senza dover creare un file. Per approfondire l'argomento, puoi consultare la documentazione ufficiale di Arduino sulle funzioni per la gestione dei file.

## Vedi anche:
Per ulteriori informazioni sull'utilizzo di file temporanei in Arduino, puoi visitare il seguente link: [https://www.arduino.cc/en/Reference/FileCreateTempFile](https://www.arduino.cc/en/Reference/FileCreateTempFile)