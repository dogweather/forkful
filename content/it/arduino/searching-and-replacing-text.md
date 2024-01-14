---
title:                "Arduino: Ricerca e sostituzione di testo"
programming_language: "Arduino"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/arduino/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Perché
Ci sono molteplici motivi per cui si potrebbe voler utilizzare la sostituzione di testo all'interno di un programma Arduino. Potrebbe essere necessario rimuovere delle stringhe di codice inutilizzate, aggiornare una variabile o semplicemente migliorare la leggibilità del codice.

## Come fare
La sostituzione di testo in un programma Arduino può essere fatta utilizzando il comando `replace()` della libreria `String`. Vediamo un esempio pratico:

```Arduino
// Definiamo una stringa
String testo = "Ciao, sono Arduino.";

// Utilizziamo il comando replace per sostituire la parola "Arduino" con "italiano"
testo.replace("Arduino", "italiano");

// Stampiamo il nuovo testo
Serial.println(testo);
```
Output:
```
Ciao, sono italiano.
```

## Approfondimento
Quando si utilizza il comando `replace()`, è importante notare che il testo originale viene modificato direttamente, senza creare una nuova stringa. Inoltre, è possibile sostituire più occorrenze di una parola se si specifica un terzo parametro opzionale, indicando il numero massimo di sostituzioni. Ad esempio:

```Arduino
// Definiamo una stringa
String testo = "Il numero di telefono di Mario è 123456789.";

// Utilizziamo il comando replace per sostituire tutti i numeri con "XXX"
testo.replace("123456789", "XXX", 3);

// Stampiamo il nuovo testo
Serial.println(testo);
```
Output:
```
Il numero di telefono di Mario è XXXXXX789.
```

## Vedi anche
- [Documentazione ufficiale su replace()](https://www.arduino.cc/reference/en/language/variables/data-types/string/functions/replace/)
- [Esempi di utilizzo di replace()](https://programmingwitharduino.com/string-replace-function-arduino/)