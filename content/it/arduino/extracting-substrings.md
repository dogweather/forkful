---
title:                "Arduino: Estrazione di sottostringhe"
programming_language: "Arduino"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/arduino/extracting-substrings.md"
---

{{< edit_this_page >}}

## Perché

L'estrazione delle sottostringhe è un'operazione molto utile nella programmazione di Arduino. Permette di ottenere una parte specifica di una stringa più grande, rendendo più facile il lavoro con i dati e semplificando il codice.

## Come fare

Per estrarre una sottostringa in Arduino, è necessario utilizzare la funzione `substring()`. Questa funzione richiede due parametri: il primo è l'indice della posizione iniziale della sottostringa, mentre il secondo è la lunghezza della sottostringa desiderata.

Per esempio, se abbiamo una stringa "Ciao Arduino" e vogliamo estrarre solo la parola "Arduino", dobbiamo utilizzare la funzione `substring(5, 7)` in modo da indicare l'indice 5 come punto di inizio e 7 come lunghezza della sottostringa.

Ecco un esempio completo di codice con l'uso della funzione `substring()`:

```arduino
String str = "Ciao Arduino";
String sottostringa = str.substring(5, 7);
Serial.print(sottostringa);
```

L'output di questo codice sarà "Arduino".

## Approfondimento

La funzione `substring()` non solo può essere utilizzata per estrarre parti di una stringa, ma anche per copiare una stringa in un'altra variabile. Questo può essere utile se si vuole manipolare la stringa originale senza modificarla direttamente.

Inoltre, è possibile utilizzare anche la funzione `indexOf()` per trovare la posizione di una specifica sottostringa all'interno di una stringa più grande, e poi utilizzare questo valore come indice nella funzione `substring()`.

## Vedi anche

- [Documentazione ufficiale di Arduino su substring()](https://www.arduino.cc/reference/it/language/variables/data-types/string/substring/)
- [Esempi di codice con l'uso di substring()](https://www.arduino.cc/en/Tutorial/BuiltInExamples/StringSubstring)
- [Tutorial di programmazione Arduino](https://www.arduino.cc/en/Tutorial/HomePage) (in italiano)