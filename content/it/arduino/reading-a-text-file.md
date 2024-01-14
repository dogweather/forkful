---
title:                "Arduino: Lettura di un file di testo"
simple_title:         "Lettura di un file di testo"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/arduino/reading-a-text-file.md"
---

{{< edit_this_page >}}

#Perché

Ci sono molte buone ragioni per voler leggere un file di testo utilizzando Arduino. Una delle più comuni è quella di usare i dati contenuti nel file per controllare i suoi circuiti e sensori. Ad esempio, potresti voler leggere un file di testo contenente i dati di temperatura per attivare un sistema di raffreddamento.

#Come fare

Per leggere un file di testo utilizzando Arduino, ci sono diversi passaggi da seguire. In primo luogo, è necessario creare un oggetto di tipo File per rappresentare il file di testo. Ciò può essere fatto utilizzando la funzione open() e specificando il nome del file e la sua modalità di apertura come parametri.

```Arduino
File file = SD.open("dati.txt", FILE_READ);
```
Una volta creato l'oggetto File, puoi utilizzare la funzione available() per verificare se il file è stato aperto con successo. In caso affermativo, puoi utilizzare la funzione read() per leggere un singolo carattere alla volta dal file e usarlo per il tuo codice.

```Arduino
while(file.available()){
  char carattere = file.read();
  //codice per utilizzare il carattere letto
}
```

Infine, una volta che hai finito di leggere il file, è importante chiuderlo utilizzando la funzione close() per liberare la memoria utilizzata dall'oggetto File.

#Approfondimento

Se vuoi fare un salto di qualità e imparare di più sulla lettura dei file di testo con Arduino, esistono diverse risorse disponibili online. Puoi iniziare con la documentazione ufficiale di Arduino sulla lettura e la scrittura dei file su SD card. Ci sono anche molti esempi e tutorial disponibili su siti e forum di appassionati di Arduino.

#Vedi anche

- [Documentazione ufficiale di Arduino sulla lettura e la scrittura dei file](https://www.arduino.cc/en/Tutorial/ReadWrite)
- [Tutorial su come leggere file di testo su Arduino](https://www.circuitsathome.com/mcu/reading-files-from-arduino-sd-card)
- [Esempi di lettura file su Arduino](https://www.arduino.cc/en/uploads/Tutorial/SimpleRead.zip)