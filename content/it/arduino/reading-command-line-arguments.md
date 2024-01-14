---
title:                "Arduino: Leggere gli argomenti dalla linea di comando"
simple_title:         "Leggere gli argomenti dalla linea di comando"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/arduino/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Perché 

Capita spesso che ci troviamo ad affrontare problemi di programmazione che richiedono l'utilizzo di input da parte dell'utente. Ciò può essere problematico in alcuni casi, specialmente se non siamo esperti di programmazione. Fortunatamente, esiste un modo semplice per gestire questo tipo di problemi: leggere gli argomenti dalla riga di comando. In questo articolo spiegheremo come fare usando Arduino.

## Come fare

Per prima cosa, è importante sapere come funzionano gli argomenti della riga di comando in Arduino. Fondamentalmente, la riga di comando è una forma di input che permette all'utente di inserire dei comandi o dei dati attraverso la tastiera. Su Arduino, è possibile leggere questi argomenti utilizzando la funzione "Serial.read()". 

Ecco un semplice esempio di codice che legge un argomento dalla riga di comando e lo stampa sulla console:

```Arduino
String input; // dichiarazione di una variabile di tipo stringa
void setup() {
  Serial.begin(9600); // inizializza la comunicazione seriale
}
void loop() {
  if (Serial.available()) { // controlla se ci sono dati disponibili sulla porta seriale
    input = Serial.readStringUntil('\n'); // legge l'input fino a quando trova un "invio"
    Serial.println(input); // stampa l'input sulla console
  }
}
```

In questo esempio, la funzione "readStringUntil()" legge i dati provenienti dalla riga di comando fino a quando non trova il carattere "\n", che rappresenta un "invio". L'input viene quindi assegnato alla variabile "input" e stampato sulla console utilizzando la funzione "println()". 

## Approfondimento

Oltre alla funzione "readStringUntil()", esistono altri modi per leggere gli argomenti dalla riga di comando in Arduino. Una di queste è la funzione "parseInt()", che permette di leggere e convertire un intero dalla riga di comando. Ecco un semplice esempio di codice che utilizza questa funzione:

```Arduino
int input; // dichiarazione di una variabile di tipo intero
void setup() {
  Serial.begin(9600); // inizializza la comunicazione seriale
}
void loop() {
  if (Serial.available()) { // controlla se ci sono dati disponibili sulla porta seriale
    input = Serial.parseInt(); // legge l'input come intero
    Serial.println(input); // stampa l'input sulla console
  }
}
```

## Vedi anche

Per maggiori informazioni su come leggere gli argomenti dalla riga di comando in Arduino, puoi consultare i seguenti link:

- https://forum.arduino.cc/index.php?topic=485792.0
- https://www.arduino.cc/en/serial/readstringuntil
- https://www.arduino.cc/en/serial/parseint