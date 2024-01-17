---
title:                "Concatenazione di stringhe"
html_title:           "Arduino: Concatenazione di stringhe"
simple_title:         "Concatenazione di stringhe"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/arduino/concatenating-strings.md"
---

{{< edit_this_page >}}

## Cosa & Perché?

La concatenazione di stringhe è un'operazione comune che i programmatori devono conoscere. Si tratta di un modo per unire due o più stringhe in una sola. Ciò è spesso utile quando si vuole creare un unico messaggio o una variabile che contiene più informazioni. 

## Come fare:

Ecco un esempio di come concatenare due stringhe in un programma Arduino:

```arduino
// Dichiarare due stringhe
String saluto = "Ciao";
String nome = "Marco";

// Effettuare la concatenazione
String messaggio = saluto + " " + nome;

// Stampare il messaggio
Serial.println(messaggio);
```
L'output di questo programma sarà "Ciao Marco" nella console seriale.

## Approfondimento:

La concatenazione di stringhe è comune nelle lingue di programmazione ed è spesso utilizzata per creare messaggi personalizzati. In alternativa, si può utilizzare la funzione `concat()` per concatenare stringhe in Arduino. Questa funzione è utile quando si vuole unire più di due stringhe. 

L'implementazione di base della concatenazione di stringhe è quella di unire due stringhe in una nuova variabile. Tuttavia, si possono anche concatenare più di due stringhe o utilizzare operatori diversi per unire le stringhe. È importante prestare attenzione alla lunghezza delle stringhe e ai limiti dei buffer per evitare errori e problemi di memoria. 

## Vedi anche:

- [Documentazione ufficiale di Arduino su Stringhe](https://www.arduino.cc/reference/en/language/variables/data-types/string/concat/)
- [Tutorial su concatenazione di stringhe in Arduino](https://www.arduino.cc/en/Tutorial/StringConcatenation)
- [Spiegazione su come funzionano le stringhe in C/C++](http://www.cplusplus.com/doc/tutorial/ntcs/)