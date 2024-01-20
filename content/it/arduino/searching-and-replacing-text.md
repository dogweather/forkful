---
title:                "Ricerca e sostituzione del testo"
html_title:           "Arduino: Ricerca e sostituzione del testo"
simple_title:         "Ricerca e sostituzione del testo"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/arduino/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Cos'è e Perché?
Ricercare e sostituire il testo sono tecniche comuni nello sviluppo di software; consentono di trovare stringhe di testo specifiche e di cambiarle con un'altra. Questo è utile per correggere errori, aggiornare terminologia o manipolare dati.

## Come fare:
La funzione `replace()` di Arduino può essere utilizzata per cercare e sostituire testo. Ecco un esempio:
```Arduino
String myString = "Ciao, mondo!";
myString.replace("mondo", "Arduino");
Serial.println(myString);  // output: Ciao, Arduino!
```
In questo snippet, la struttura del codice `myString.replace("mondo","Arduino")` cerca la parola "mondo" e la sostituisce con "Arduino".

## Esercizio Approfondito
La ricerca e la sostituzione del testo risalgono ai primi giorni del computing, quando gli editor di testo come vi e emacs includevano potenti funzioni di grep. Le alternative a `replace()` in Arduino sono spesso più complesse, ad esempio l'uso di funzioni come `indexOf()` e `substring()`. Detto ciò, `replace()` è una funzione molto utile ma semplice da utilizzare. La sua implementazione interna utilizza due puntatori per scorrere la stringa, trovando corrispondenze e sovrascrivendo dove necessario.

## Per saperne di più
Per ulteriori dettagli sulla programmazione con Arduino in italiano, i seguenti links possono essere utili:
- [Documentazione Ufficiale di Arduino](https://www.arduino.cc/reference/it/)
- [Forum Italiano di Arduino](http://forum.arduino.cc/index.php?board=33.0)