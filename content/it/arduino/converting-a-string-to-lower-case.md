---
title:    "Arduino: Convertire una stringa in minuscolo"
keywords: ["Arduino"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/it/arduino/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Perché

Convertire una stringa in minuscolo è un'operazione comune nella programmazione. Ad esempio, se si sta creando un programma per monitorare la temperatura di una serra, è utile avere la possibilità di inserire la temperatura iniziale in lettere minuscole nel codice, invece di doverla digitare manualmente ogni volta.

## Come

Per convertire una stringa in minuscolo in Arduino, possiamo utilizzare la funzione `toLowerCase()`. Di seguito è riportato un esempio di codice che prende una stringa di testo, la converte in minuscolo e la stampa nel monitor seriale:

```arduino
String testo = "QUESTA E' UNA STRINGA DI TESTO IN MAIUSCOLO";
String testoConvertito = testo.toLowerCase();
Serial.println(testoConvertito);
```

L'output di questo codice sarà "questa e' una stringa di testo in maiuscolo".

## Approfondimento

La funzione `toLowerCase()` è disponibile per l'oggetto String in Arduino, che rappresenta una stringa di testo. Essa converte tutti i caratteri della stringa in minuscolo e restituisce una nuova stringa con il risultato.

Un altro modo per convertire una stringa in minuscolo è utilizzare la libreria standard `ctype.h`. Questa libreria fornisce la funzione `tolower()` che prende in input un carattere e lo converte in minuscolo. Di seguito è riportato un esempio di codice che utilizza questa funzione per convertire una stringa in minuscolo:

```arduino
#include <ctype.h>

String testo = "QUESTO E' UN ALTRO ESEMPIO";
String testoConvertito = "";

for (int i = 0; i < testo.length(); i++) {
  testoConvertito += (char)tolower(testo.charAt(i)); 
}

Serial.println(testoConvertito);
```

L'output di questo codice sarà "questo e' un altro esempio".

## Vedi anche

- [Funzione toLowerCase() - Reference Arduino](https://www.arduino.cc/reference/it/language/variables/data-types/string/functions/tolowercase/)
- [Libreria standard ctype.h - Tutorialspoint](https://www.tutorialspoint.com/c_standard_library/ctype_h.htm)
- [Funzione tolower() - Tutorialspoint](https://www.tutorialspoint.com/c_standard_library/c_function_tolower.htm)