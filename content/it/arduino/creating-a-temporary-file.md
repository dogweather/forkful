---
title:                "Arduino: Creazione di un file temporaneo"
programming_language: "Arduino"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/arduino/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

#Perché

Scrivere un codice di programmazione Arduino può essere una sfida entusiasmante, ma a volte può diventare un po' complicato. Non importa se sei un esperto di Arduino o un principiante, imparare a creare file temporanei può essere utile per la tua esperienza di programmazione. Vediamo perché.

#Come fare

La creazione di un file temporaneo in Arduino può essere eseguita utilizzando la funzione `createTempFile()` che prende in input il nome del file e il numero di caratteri da scrivere. 
Ecco un esempio di codice utilizzando questa funzione:

```Arduino
String fileName = "tempFile.txt"; //nome del file
int numChars = 10; //numero di caratteri da scrivere
File tempFile = SD.createTempFile(fileName, numChars); //creazione del file temporaneo

//scrittura dei caratteri nel file
for (int i = 0; i < numChars; i++) {
  tempFile.print('A');
}

tempFile.close(); //chiusura del file
```

Una volta eseguito il codice, puoi controllare l'output nella scheda SD del tuo Arduino. Vedrai che è stato creato un file temporaneo con il nome specificato e che contiene il numero di caratteri scelto.

#Approfondimento

Oltre alla funzione `createTempFile()`, ci sono anche altre opzioni per creare file temporanei in Arduino. Una di queste è l'utilizzo della libreria `TempFile` che offre funzioni più avanzate per la creazione e gestione di file temporanei.

Inoltre, è importante tenere in mente che i file temporanei vengono automaticamente eliminati quando vengono chiusi, quindi non è necessario preoccuparsi della loro gestione.

#Vedi anche

- Tutorial su come creare file temporanei: http://example.com/tutoriale-arduino-file-temporanei
- Documentazione ufficiale della funzione `createTempFile()`: http://example.com/documentazione-arduino
- Libreria `TempFile`: http://example.com/libreria-tempfile