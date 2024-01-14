---
title:                "Arduino: Maiuscolare una stringa"
programming_language: "Arduino"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/arduino/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Perché

Capitare una stringa potrebbe sembrare una cosa semplice, ma in realtà può essere utile in molte applicazioni. Ad esempio, potrebbe essere necessario convertire un input dall'utente tutto in maiuscolo per garantire che corrisponda a una determinata parola chiave o per facilitare la ricerca dei dati.

## Come fare

Per capitalizzare una stringa su Arduino, è possibile utilizzare la funzione `toUpperCase()`. Di seguito è riportato un esempio di codice che prende una stringa di input dall'utente e la converte in maiuscolo utilizzando questa funzione:

```Arduino
String input = Serial.readString(); //legge l'input dall'utente
input.toUpperCase(); // converte la stringa in maiuscolo
Serial.print("La stringa in maiuscolo è: ");
Serial.println(input); // stampa il risultato
```

Il risultato di questa operazione sarà la stringa originale, ma tutta in maiuscolo. Ad esempio, se l'utente inserisse "ciao", il risultato sarebbe "CIAO".

## Approfondimento

La funzione `toUpperCase()` fa parte della classe String di Arduino. Essa utilizza la tabella ASCII per convertire i caratteri in maiuscolo. Ciò significa che tutti i caratteri alfanumerici e la maggior parte dei caratteri speciali saranno convertiti correttamente, ma potrebbero esserci alcune eccezioni con caratteri di lingue diverse dall'inglese. Inoltre, questa funzione non modifica la stringa originale, ma restituisce una nuova stringa in maiuscolo.

## Vedi anche

- [`toUpperCase()` su Arduino.cc](https://www.arduino.cc/reference/it/language/variables/data-types/stringfunctions/touppercase/)
- [Tabella ASCII](https://www.ascii.cl/)