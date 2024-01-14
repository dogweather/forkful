---
title:                "Arduino: Stampa dell'output di debug"
programming_language: "Arduino"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/arduino/printing-debug-output.md"
---

{{< edit_this_page >}}

## Perché

Stampare l'output di debug è un modo utile per analizzare il comportamento del nostro codice e verificare se funziona come previsto. Ci permette di individuare e risolvere eventuali errori o bug nel nostro programma.

## Come fare

Per stampare l'output di debug in Arduino, utilizzeremo la funzione "Serial.print()". Questa funzione ci permette di scrivere un messaggio sulla porta seriale, che può essere visualizzato tramite il monitor seriale. Ecco un esempio di codice:

```Arduino
int value = 5; // dichiara una variabile "value" con valore 5
Serial.print("Il valore della variabile è: "); // stampa un messaggio di debug
Serial.println(value); // stampa il valore della variabile
```

L'output sul monitor seriale sarà:

`Il valore della variabile è: 5`

Notare che abbiamo utilizzato "println" invece di "print". Questa funzione aggiunge una nuova riga dopo l'output, rendendo più leggibile il nostro messaggio.

Possiamo anche stampare il valore di variabili all'interno di una stringa utilizzando "Serial.println()". Ad esempio:

```Arduino
int temperatura = 25; // dichiara una variabile "temperatura" con valore 25
Serial.println("La temperatura è: " + String(temperatura) + " °C"); // combina una stringa con il valore della variabile e stampa il risultato
```

L'output sul monitor seriale sarà:

`La temperatura è: 25 °C`

## Approfondimenti

La funzione "Serial.print()" può essere utilizzata in molte altre situazioni, come per stampare il valore binario di una variabile o per convertire un numero in stringa. Inoltre, è possibile utilizzare la porta seriale per comunicare con altri dispositivi esterni, come sensori o display.

## Vedi anche

- [Documentazione ufficiale di Arduino sulla funzione Serial.print()](https://www.arduino.cc/reference/it/language/functions/communication/serial/print/)
- [Tutorial di Adafruit su come utilizzare la funzione Serial.print()](https://learn.adafruit.com/lesson-5-serial-communications)
- [Esempi di progetti di Arduino che utilizzano la comunicazione seriale](https://create.arduino.cc/projecthub/search?query=serial+print)