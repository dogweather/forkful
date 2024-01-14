---
title:    "Arduino: Capitalizzare una stringa"
keywords: ["Arduino"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/it/arduino/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Perché
In questo articolo parleremo di come implementare la funzione di capitalizzazione di una stringa in Arduino. Questa operazione può essere utile in diversi contesti, ad esempio quando si devono gestire input da parte dell'utente o quando si vuole formattare correttamente un output.

## Come fare
Per implementare la funzione di capitalizzazione, utilizzeremo il metodo `toUpperCase()` della classe `String` di Arduino. Questo metodo prende in input una stringa e restituisce una nuova stringa con tutte le lettere maiuscole.

```Arduino
// Esempio di utilizzo del metodo toUpperCase()
String s = "arduino";
String s_cap = s.toUpperCase();
Serial.println(s_cap); // stampa "ARDUINO"
```

In questo modo, possiamo inserire qualsiasi stringa come input e ottenere in output una versione capitalizzata. È importante notare che questo metodo non modifica la stringa originale, ma ne crea una copia modificata.

## Approfondimento
Per comprendere meglio come funziona il metodo `toUpperCase()`, dobbiamo conoscere il concetto di array di caratteri (char array) in Arduino. Questo tipo di dato viene utilizzato per rappresentare una stringa di caratteri e ha una lunghezza fissa.

Nel nostro esempio, la stringa "arduino" viene rappresentata come un array di 7 caratteri, dove viene allocato un carattere per ogni lettera più un carattere speciale di terminazione (`'\0'`). Quando chiamiamo il metodo `toUpperCase()`, Arduino itera attraverso l'array di caratteri e, per ogni carattere, utilizza la funzione `toUpper()` della libreria standard `ctype.h`, che restituisce la versione maiuscola di quel carattere.

## Vedi anche
- Documentazione ufficiale del metodo `toUpperCase()` di Arduino: https://www.arduino.cc/reference/en/language/variables/data-types/string/functions/touppercase/
- Tutorial sulla gestione delle stringhe in Arduino: https://www.tutorialspoint.com/arduino/arduino_strings.htm
- Libreria `ctype.h` della libreria standard di C: https://www.programiz.com/c-programming/library-function/ctype.h/toupper