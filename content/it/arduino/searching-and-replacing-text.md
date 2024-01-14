---
title:    "Arduino: Ricerca e sostituzione di testo"
keywords: ["Arduino"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/it/arduino/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Perché

Se sei un programmatore Arduino, potresti aver incontrato il problema di dover sostituire testo all'interno del tuo codice. Questo può essere dovuto a vari motivi, come la correzione di errori ortografici o la modifica di una funzione specifica. In ogni caso, la ricerca e sostituzione del testo può semplificare notevolmente il processo di modifica del tuo codice.

## Come Fare

Per eseguire una ricerca e sostituzione del testo in Arduino, puoi utilizzare la funzione `replace()` che accetta tre argomenti: il testo da cercare, il testo con cui sostituirlo e la posizione da cui iniziare la ricerca. Ecco un esempio di codice:

```Arduino
replace("Hello World", "Ciao Mondo", 0);
```

Questo sostituirà il testo "Hello World" con "Ciao Mondo" all'inizio del codice. Puoi anche specificare un'area di testo più specifica aggiungendo un altro argomento per la lunghezza della stringa:

```Arduino
replace("Hello World", "Ciao Mondo", 0, 5);
```

Questo sostituirà solo i primi 5 caratteri di "Hello World" con "Ciao Mondo".

## Approfondimento

Ci sono alcune cose da tenere a mente quando si utilizza la funzione `replace()` in Arduino. Prima di tutto, questa funzione sostituisce solo la prima occorrenza del testo cercato e non tutte le occorrenze all'interno di una stringa. Inoltre, i caratteri speciali, come i backslash, possono causare problemi nella sostituzione del testo.

Inoltre, puoi utilizzare la funzione `replaceAll()` per sostituire tutte le occorrenze di un testo specifico all'interno di una stringa.

```Arduino
replaceAll("Hello Hello Hello", "Hello", "Ciao");
```

Questo sostituirà ogni occorrenza di "Hello" con "Ciao" all'interno della stringa.

## Vedi Anche

- [Documentazione ufficiale di Arduino sulla funzione replace()](https://www.arduino.cc/reference/en/language/variables/data-types/string/functions/replace/)
- [Tutorial su come utilizzare la funzione replace() in Arduino](https://create.arduino.cc/projecthub/AnuragVasanwala/arduino-replace-and-stringlength-functions-67adb8)
- [Esempi di codice per la funzione replace() in Arduino](https://www.geeksforgeeks.org/arduino-string-replace-function/)