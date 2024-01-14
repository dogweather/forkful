---
title:    "Arduino: Eliminazione di caratteri corrispondenti a un pattern"
keywords: ["Arduino"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/it/arduino/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

Cancellare i Caratteri Corrispondenti a un Modello con Arduino

## Perché

A volte può essere necessario cancellare i caratteri in una stringa che corrispondono a un certo modello. Questo potrebbe essere utile, ad esempio, quando si ricevono dati da un sensore e si desidera rimuovere una parte della stringa che non è rilevante per il progetto. In questa guida, mostrerò come è possibile fare ciò utilizzando Arduino.

## Come Fare

Per prima cosa, dobbiamo dichiarare una variabile di tipo `String` per contenere i nostri dati:

```Arduino
String data = "5-10-2021";
```

Successivamente, dobbiamo utilizzare la funzione `replace`, che sostituirà ogni occorrenza del modello con la stringa vuota, eliminando così i caratteri corrispondenti. Nel nostro esempio, vogliamo eliminare tutti i trattini:

```Arduino
data.replace("-", "");
```

Infine, possiamo stampare il risultato:

```Arduino
Serial.println(data);
```

L'output sarà `5102021`, senza alcun trattino. Ovviamente, è possibile personalizzare il modello da cercare e il testo di sostituzione in base alle proprie esigenze.

## Approfondimento

La funzione `replace` è basata sulla libreria `String`, che utilizza delle espressioni regolari per effettuare la ricerca dei pattern. Ciò significa che la sua implementazione è abbastanza sofisticata e consente di utilizzare molti modelli diversi per la ricerca e la sostituzione dei caratteri. I più curiosi possono approfondire questo argomento per scoprire tutte le opzioni disponibili.

## Vedi Anche

- [Documentazione ufficiale sulla funzione replace di Arduino](https://www.arduino.cc/reference/en/language/variables/data-types/string/functions/replace/)
- [Tutorial sulle espressioni regolari con Arduino](https://www.makerguides.com/arduino-regular-expressions/)