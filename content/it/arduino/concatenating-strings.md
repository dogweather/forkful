---
title:    "Arduino: Unione di stringhe"
keywords: ["Arduino"]
---

{{< edit_this_page >}}

## Perché

Hai mai avuto la necessità di unire due o più parole o frasi nel tuo codice Arduino? Forse vuoi creare un messaggio dinamico o semplicemente avere più controllo sulla visualizzazione di dati. In entrambi i casi, la concatenazione di stringhe può essere una tecnica utile da utilizzare nel tuo programma.

## Come Fare

La concatenazione di stringhe in Arduino è abbastanza semplice. Basta utilizzare l'operatore "+" tra le stringhe che si desidera unire. Vediamo un esempio di codice e l'output corrispondente:

```Arduino
String greeting = "Ciao";
String name = "Marta";
String message = greeting + " " + name + "!";
Serial.println(message);
```

L'output di questo codice sarà:

```Arduino
Ciao Marta!
```

In questo esempio, abbiamo utilizzato l'operatore "+" per concatenare le stringhe "Ciao" e "Marta" con uno spazio vuoto tra di esse. Ora possiamo facilmente personalizzare il nostro messaggio di saluto utilizzando il valore della variabile "name".

## Approfondimento

Mentre la concatenazione di stringhe può sembrare semplice, è importante comprendere come funziona esattamente. Quando usiamo l'operatore "+", Arduino in realtà crea una nuova stringa contenente i contenuti delle stringhe originali. Questo può diventare un problema se le stringhe che stai unendo sono molto lunghe o se stai eseguendo molteplici concatenazioni in un breve periodo di tempo.

Per evitare problemi di memoria e prestazioni, è consigliabile utilizzare la classe String di Arduino solo quando è necessario e non per gestire grandi quantità di dati. Inoltre, se stai utilizzando molteplici concatenazioni, potresti voler considerare l'utilizzo di un'altra funzione, come "strcat()", che è progettata specificamente per l'unione di più stringhe.

## Vedi Anche

- [Tutorial su concatenazione di stringhe in Arduino](https://www.arduino.cc/en/Tutorial/StringAdditionOperator)
- [Documentazione sull'utilizzo della classe String in Arduino](https://www.arduino.cc/reference/en/language/variables/data-types/string/)
- [Esempi pratici di concatenazione di stringhe in Arduino](https://www.teachmemicro.com/arduino-concatenate-strings/)