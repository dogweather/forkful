---
title:    "Arduino: Leggere un file di testo"
keywords: ["Arduino"]
---

{{< edit_this_page >}}

## Perchè

Leggere un file di testo può essere utile per l'interazione tra Arduino e un dispositivo esterno, come un computer o un modulo WiFi. Potrebbe essere necessario leggere dei dati da un file per completare un progetto o per salvare informazioni.

## Come

Per leggere un file di testo su Arduino, segui questi semplici passi:

1. Connetti Arduino al dispositivo esterno tramite un cavo USB o un modulo WiFi.
2. Assicurati che il tuo codice includa la libreria `SD.h` per permettere ad Arduino di leggere un file dalla scheda SD.
3. Usa la funzione `SD.open()` per aprire il file specificato. Passa il nome del file e la modalità di accesso come argomenti. Ad esempio, `SD.open("data.txt", FILE_READ)`.
4. Utilizza il metodo `read()` per leggere i dati dal file aperto. Questo restituirà un intero che rappresenta il byte letto dal file.
5. Ripeti il metodo `read()` fino a quando non hai ottenuto tutti i dati dal file.

Ecco un esempio di codice Arduino per leggere un file di testo chiamato "data.txt" e stamparne il contenuto sulla porta seriale:

```
#include <SPI.h>
#include <SD.h>

File data;

void setup() {
  Serial.begin(9600);
  
  if (!SD.begin(4)) {
    Serial.println("Errore inizializzazione SD!");
    while (true);
  }

  data = SD.open("data.txt", FILE_READ);

  if (data) {
    while (data.available()) {
      Serial.write(data.read());
    }
  }
  data.close();
}

void loop() {}
```

Se il file "data.txt" contiene la stringa "Ciao mondo!", il risultato sulla porta seriale sarà "Ciao mondo!".

## Approfondimento

Ci sono diverse cose da tenere presente quando si legge un file di testo su Arduino:

- Assicurati che la libreria `SD.h` sia correttamente installata e inclusa nel tuo codice. Altrimenti, riceverai un errore di compilazione.
- Utilizza la modalità di accesso `FILE_READ` quando apri un file per la lettura.
- Se il tuo file contiene caratteri speciali o linee vuote, potresti ottenere dei dati inaspettati. Assicurati che il file sia ben formattato prima di leggerlo.
- Se utilizzi un modulo WiFi per connettere Arduino a un server esterno, assicurati che il server sia configurato per consentire l'accesso al file di testo.

## Vedi Anche

- [Documentazione ufficiale sulla libreria SD.h](https://www.arduino.cc/en/Reference/SD)
- [Esempi di codice per leggere un file di testo su Arduino](https://www.arduino.cc/en/Tutorial/ReadASCIIString)
- [Guida su come leggere e scrivere un file di testo con Arduino](https://www.makerspaces.com/how-to-read-and-write-sd-card-with-arduino/)