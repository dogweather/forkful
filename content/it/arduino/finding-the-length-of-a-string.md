---
title:                "Arduino: Trovare la lunghezza di una stringa."
simple_title:         "Trovare la lunghezza di una stringa."
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/arduino/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Perché

Ci sono molte volte in cui avremmo bisogno di conoscere la lunghezza di una stringa in Arduino, come per esempio nell'analisi di un input da un sensore o nel salvataggio di dati in un array. Conoscere la lunghezza della stringa ci consente di gestire al meglio i nostri dati e creare codice più efficiente.

## Come fare

Calcolare la lunghezza di una stringa è un'operazione abbastanza semplice in linguaggio C e quindi anche in Arduino. Iniziamo creando una nuova variabile di tipo `char` che conterrà la nostra stringa e assegniamole il valore desiderato:

```Arduino
char miaStringa[] = "Ciao mondo!";
```

Una volta definita la nostra stringa, possiamo utilizzare la funzione `strlen()` per ottenere la sua lunghezza:

```Arduino
int lunghezza = strlen(miaStringa);
Serial.println(lunghezza); // output: 11
```

Come possiamo vedere, la funzione `strlen()` restituisce un valore intero che rappresenta la lunghezza della stringa in caratteri. Ovviamente, questo valore può variare a seconda della lunghezza della stringa che abbiamo definito.

## Approfondimento

Se vogliamo capire meglio come funziona la funzione `strlen()` e come possiamo utilizzare al meglio questa conoscenza, possiamo analizzare il suo codice sorgente. La funzione `strlen()` fa parte della libreria standard del linguaggio C e la sua definizione è la seguente:

```Arduino
size_t strlen(const char* str) {
    const char* s;
    for (s = str; *s; ++s);
    return(s - str);
}
```

In termini semplici, questa funzione scandisce ogni carattere della stringa finché non trova un carattere nullo `\0`, che indica la fine della stringa. A questo punto, la funzione restituisce la differenza tra la posizione corrente e quella iniziale, ovvero la lunghezza della stringa.

Utilizzando questa conoscenza, possiamo creare delle varianti della funzione `strlen()` che, ad esempio, restituiscono il numero di vocali o di consonanti presenti nella stringa.

## Vedi anche

- [Documentazione ufficiale di Arduino](https://www.arduino.cc/reference/en/language/variables/data-types/string/functions/strlen/)
- [Guida alla programmazione in linguaggio C](https://www.tutorialspoint.com/cprogramming/c_strings.htm)
- [Esempi di utilizzo della funzione `strlen()`](https://www.geeksforgeeks.org/strlen-function-in-c/)
- [Approfondimento sulla manipolazione delle stringhe in Arduino](https://www.arduino.cc/en/Tutorial/StringLengthTrim)