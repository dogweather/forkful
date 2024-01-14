---
title:    "Arduino: Stampa dell'output di debug"
keywords: ["Arduino"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/it/arduino/printing-debug-output.md"
---

{{< edit_this_page >}}

## Perché

Spesso durante lo sviluppo di progetti Arduino, è necessario comprendere cosa sta accadendo all'interno del nostro codice. Qui entra in gioco l'uso della stampa dell'output di debug. Con la stampa di output di debug, è possibile ottenere informazioni dettagliate sul comportamento del nostro codice e utilizzarle per risolvere eventuali errori o problemi.

## Come fare

Per stampare l'output di debug su Arduino, è necessario utilizzare la funzione `Serial.println()`. Questa funzione accetta come parametro un valore o una stringa da stampare sulla console seriale. Vediamo un esempio di come utilizzarla:

```Arduino
int valore = 5;
Serial.println("Il valore è: ");
Serial.println(valore);
```

Questa sequenza di codice stamperà sulla console seriale la stringa "Il valore è: " seguita da un'altra riga contenente il valore della variabile `valore`, che in questo caso è 5.

## Approfondimento

La funzione `Serial.println()` è particolarmente utile quando si lavora con sensori o moduli esterni. Ad esempio, si può utilizzare per visualizzare i valori letti da un sensore di temperatura o per verificare il corretto funzionamento di un modulo.

È possibile utilizzare anche la funzione `Serial.print()` che ha la stessa funzionalità di `Serial.println()`, ma non aggiunge un carattere di ritorno a capo automaticamente alla fine della stampa. Inoltre, è possibile specificare il numero di cifre dopo la virgola da stampare utilizzando la funzione `Serial.println(valore, numeroCifre);`.

## Vedi anche

- [Documentazione ufficiale di Arduino](https://www.arduino.cc/reference/en/language/functions/communication/serial/print/)
- [Tutorial su come utilizzare la stampa di output di debug su Arduino](https://www.arduino.cc/en/Tutorial/Foundations/PrintDebugging)