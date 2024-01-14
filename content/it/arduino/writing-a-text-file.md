---
title:    "Arduino: Scrivere un file di testo"
keywords: ["Arduino"]
---

{{< edit_this_page >}}

## Perché

Scrivere un file di testo è un'operazione comune nei programmi di Arduino. Può essere utilizzato per registrare dati, archiviare informazioni o semplicemente visualizzare output su un display.

## Come fare

Per scrivere un file di testo su Arduino, è necessario utilizzare la funzione 'File' e specificare il nome del file e il percorso in cui si desidera salvarlo. Di seguito è riportato un esempio di codice che scriverà una semplice stringa di testo in un file di nome "test.txt" nella cartella principale dell'Arduino.

```
Arduino void loop() {
    File file = SD.open("test.txt", FILE_WRITE);
    if (file) {
        file.println("Ciao lettori italiani!");
        file.close();
    }
}
```

Una volta che il codice viene eseguito, il file di testo verrà creato e lo vedrete apparire tra i file nella cartella principale dell'Arduino.

## Approfondimento

Oltre alla semplice operazione di scrivere una stringa di testo in un file, è possibile anche leggere e scrivere in posizioni specifiche del file, creare e cancellare file, e altro ancora utilizzando le funzioni della libreria SD della scheda SD di Arduino.

Per maggiori informazioni sulle possibilità di scrivere file su Arduino, si consiglia di consultare la documentazione ufficiale di Arduino o di cercare esempi di codice online.

## Vedi anche

- [Documentazione ufficiale di Arduino](https://www.arduino.cc/)
- [Esempi di codice per scrivere file su Arduino](https://www.arduino.cc/search?q=writing%20file)