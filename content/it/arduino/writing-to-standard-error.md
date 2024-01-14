---
title:    "Arduino: Scrivere su standard error"
keywords: ["Arduino"]
---

{{< edit_this_page >}}

##Perché
Scrivere su standard error è un metodo utile per verificare e risolvere gli errori nel codice Arduino. Quando si utilizza questo metodo, gli errori vengono visualizzati direttamente sul monitor seriale, rendendo più facile individuare e correggere eventuali errori nel codice.

##Come Fare
Per scrivere su standard error in Arduino, è necessario utilizzare la funzione ```Arduino Serial.println()```. Questa funzione accetta come parametro un messaggio o il valore di una variabile e lo stampa sul monitor seriale. Di seguito è riportato un esempio di codice che stampa un messaggio di errore utilizzando questa funzione:

```
Arduino Serial.println("Errore: valore non valido");
```

L'output di questo codice sarà "Errore: valore non valido" visualizzato sul monitor seriale. In questo modo, è possibile identificare facilmente il punto nel codice in cui si è verificato l'errore.

##Analisi Approfondita
Quando si scrive su standard error, è importante ricordare di utilizzare questa funzione solo per gli errori veri e propri e non per la normale stampa di messaggi o valori nel codice. Inoltre, per una migliore leggibilità del codice, è consigliato utilizzare delle costanti per gli eventuali messaggi di errore, in modo da poter modificare facilmente il messaggio in caso di necessità.

Un altro aspetto da tenere in considerazione è la scelta dei caratteri da utilizzare nella stampa degli errori. È consigliato utilizzare dei caratteri speciali come "\n" per andare a capo o "\t" per aggiungere un tabulatore, in modo da rendere più leggibile l'output sul monitor seriale.

##Vedi Anche
- [Guida all'utilizzo del monitor seriale in Arduino](https://www.arduino.cc/reference/en/language/functions/communication/serial/)
- [Esempi di utilizzo della funzione Serial.println()](https://lastminuteengineers.com/arduino-serial-println-function/)
- [Tutorial sul debug del codice Arduino](https://www.arduino.cc/en/Tutorial/Debugger)