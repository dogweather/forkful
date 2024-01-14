---
title:                "Arduino: Capitalizzare una stringa"
simple_title:         "Capitalizzare una stringa"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/arduino/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Perché
Molti progetti di Arduino richiedono la manipolazione di stringhe di testo, ma a volte è necessario modificare solo la prima lettera di una parola o frase in maiuscolo. In questo caso, è utile conoscere come capitalizzare una stringa di testo in Arduino. 

## Come fare
Per capitalizzare una stringa in Arduino, è necessario utilizzare la funzione `toupper()`. Inizialmente, si deve dichiarare la stringa da modificare e poi utilizzare un ciclo `for` per iterare attraverso ogni carattere della stringa. All'interno del ciclo, utilizzare la funzione `toupper()` per convertire ogni carattere in maiuscolo. Di seguito, è un esempio di codice che capitalizza la stringa "ciao mondo" e la stampa su seriale.

```Arduino
String stringa = "ciao mondo";
for (int i = 0; i < stringa.length(); i++) {
  stringa[i] = toupper(stringa[i]);
}
Serial.println(stringa);
```

Il risultato sarà:

```Arduino
CIAO MONDO
```

## Approfondimento
La funzione `toupper()` utilizzata nell'esempio fa parte della libreria standard di C++. Essa accetta un carattere come parametro e restituisce il corrispondente carattere maiuscolo. Tuttavia, questa funzione non modifica il valore originale del carattere, ma ne restituisce solo una copia modificata. Per questo motivo, è necessario assegnare il risultato della funzione a una variabile o utilizzarlo direttamente come parametro in una funzione di output, come `Serial.println()`. 

## Vedi anche
- [Documentazione di Arduino su la funzione `toupper()`](https://www.arduino.cc/reference/en/language/variables/data-types/toupper/)
- [Esempi di codice di Arduino](https://www.arduino.cc/en/Tutorial/HomePage)