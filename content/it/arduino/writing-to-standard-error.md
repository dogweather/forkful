---
title:                "Arduino: Scrivere su standard error"
programming_language: "Arduino"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/arduino/writing-to-standard-error.md"
---

{{< edit_this_page >}}

##Perché

Scrivere su standard error è un importante aspetto della programmazione su Arduino che può aiutare a identificare e risolvere errori nel codice. È un modo per visualizzare messaggi di errore specifici che possono aiutare a debuggare e ottimizzare il programma.

##Come Fare

Per scrivere su standard error in Arduino, è necessario utilizzare la funzione `Serial.println()`. Questo comando invia un messaggio di testo alla porta seriale USB del microcontrollore, che può essere visualizzato in un terminale seriale sul computer.

Ecco un esempio di codice che scrive un messaggio di errore alla fine del programma:

```Arduino
void setup() {
  Serial.begin(9600);  // inizializza la comunicazione seriale
}

void loop() {
  // codice del programma

  // se si verifica un errore, scrivere il messaggio su standard error
  Serial.println("Errore: variabile non definita");

  // altri comandi del programma
}
```

L'output di questo esempio verrà visualizzato nel terminale seriale come:

`Errore: variabile non definita`

##Approfondimento

Per massimizzare l'utilità di scrivere su standard error, è importante comprendere come gestire i messaggi di errore nel tuo codice. Ecco alcune considerazioni:

- Utilizzare messaggi di errore chiari e descrittivi per aiutare a identificare il problema facilmente.
- Aggiungere numeri di riga o altre informazioni nel messaggio di errore per individuare più facilmente il punto esatto in cui si è verificato l'errore.
- Utilizzare la funzione `Serial.setTimeout()` per impostare un limite di tempo per leggere i messaggi di errore, in modo da non bloccare il programma in caso di errore.

##Vedi Anche

- [Documentazione Arduino su Serial.println()](https://www.arduino.cc/reference/en/language/functions/communication/serial/println/)
- [Tutorial su come scrivere messaggi di errore efficaci in Arduino](https://create.arduino.cc/projecthub/eduardoschneiders/arduino-tutorial-writing-effective-error-messages-e41d82)