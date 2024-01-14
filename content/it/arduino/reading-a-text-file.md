---
title:                "Arduino: Lettura di un file di testo"
programming_language: "Arduino"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/arduino/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Perché

Leggere un file di testo è fondamentale quando si lavora con Arduino. Questo processo consente di acquisire dati esterni e utilizzarli all'interno del programma, consentendo una maggiore versatilità e possibilità di personalizzazione dei progetti. In questo articolo scoprirai come leggere un file di testo utilizzando Arduino.

## Come fare

Per iniziare, avrai bisogno di un Arduino e un file di testo contenente i dati che desideri utilizzare. Segui questi semplici passaggi per leggere il file di testo:

1. Collega il tuo Arduino al computer utilizzando un cavo USB.
2. Apri l'IDE di Arduino sul tuo computer.
3. Copia e incolla il seguente codice nell'IDE:

```Arduino
// Definizione delle variabili
File file;
String data;

void setup() {
  // Avvia la comunicazione seriale
  Serial.begin(9600);

  // Apri il file di testo
  file = SD.open("file_testo.txt");

  // Leggi il contenuto del file
  while (file.available()) {
    data = file.readString();
    Serial.println(data);
  }

  // Chiudi il file
  file.close();

}

void loop() {
  // Inserisci qui eventuali altre attività da eseguire
}
```
4. Modifica il nome del file di testo nella riga `file = SD.open("file_testo.txt");` con il nome del tuo file.
5. Carica il codice sul tuo Arduino.
6. Apri il monitor seriale nella finestra dell'IDE di Arduino per visualizzare i dati letti dal file di testo.

## Approfondimento

La funzione `SD.open()` viene utilizzata per aprire il file di testo e restituisce un oggetto di tipo `File`, che contiene il contenuto del file. A questo punto, il contenuto del file viene letto e memorizzato nella variabile `data` utilizzando la funzione `readString()`. Infine, il file viene chiuso utilizzando la funzione `close()`.

È importante ricordare che il file di testo deve essere posto nella stessa cartella del file di sketch di Arduino. Inoltre, il file deve essere formattato correttamente per essere letto correttamente dal codice.

In aggiunta ai dati, è possibile anche leggere informazioni sul file stesso, come il nome, il percorso e la dimensione utilizzando le funzioni `name()`, `fileSize()` e `dirPath()` rispettivamente.

## Vedi anche

- [Tutorial: Leggere un file di testo con Arduino](https://www.arduino.cc/en/Tutorial/ReadASCIIString)
- [Funzione `SD.open()` di Arduino](https://www.arduino.cc/en/Reference/SDopen)
- [Esempi di codice per la lettura di file di testo con Arduino](https://github.com/arduino-libraries/SD/blob/master/examples/ReadWrite/ReadWrite.ino)