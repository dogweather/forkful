---
title:                "Arduino: Trovare la lunghezza di una stringa"
programming_language: "Arduino"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/arduino/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Perché
Cosa ti spinge a voler conoscere la lunghezza di una stringa nel tuo programma Arduino? Forse vuoi verificare la correttezza dei tuoi input o semplicemente gestire meglio la memoria disponibile sul tuo dispositivo. Qualunque sia la tua motivazione, il seguente tutorial ti guiderà attraverso il processo di trovare la lunghezza di una stringa in modo semplice ed efficiente.

## Come fare
Innanzitutto, è necessario comprendere cosa si intende per "lunghezza di una stringa". In informatica, una stringa è semplicemente un insieme di caratteri, come lettere o numeri, messi insieme in un certo ordine. La lunghezza di una stringa si riferisce quindi al numero di caratteri presenti in essa.

Per trovare la lunghezza di una stringa nel tuo codice Arduino, puoi utilizzare la funzione `strlen()`. Questa funzione accetta un parametro di tipo `char`, che rappresenta la stringa di cui vuoi trovare la lunghezza, e restituisce un valore di tipo `size_t`, che indica il numero di caratteri nella stringa.

Di seguito è riportato un esempio di codice che utilizza la funzione `strlen()` per trovare la lunghezza di una stringa e poi stamparla sul monitor seriale:

```Arduino
char myString[] = "Hello World";
int length = strlen(myString);
Serial.println(length); // stampa "11"
```

In questo esempio, abbiamo dichiarato una variabile di tipo `char` chiamata `myString` e gli abbiamo assegnato il valore della nostra stringa "Hello World". Poi abbiamo utilizzato la funzione `strlen()` per assegnare il valore della lunghezza della stringa alla variabile `length`. Infine, abbiamo utilizzato la funzione `Serial.println()` per stampare il valore di `length` sul monitor seriale.

## Approfondimento
Se vuoi approfondire ulteriormente come funziona la funzione `strlen()`, puoi dare un'occhiata all'implementazione di questa funzione nella libreria standard di C. Inoltre, puoi anche trovare altre funzioni utili per la manipolazione delle stringhe nella libreria `string.h`, come ad esempio `strcpy()` per copiare le stringhe o `strcat()` per concatenarle.

## Vedi anche
- [Documentazione della funzione `strlen()` su arduino.cc](https://www.arduino.cc/reference/it/language/functions/string-manipulation/strlen/)
- [Tutorial su come utilizzare le stringhe in Arduino](https://www.elegoo.com/blogs/arduino-projects/arduino-projects-basics-string-manipulation)
- [Documentazione della libreria `string.h` su cplusplus.com](https://www.cplusplus.com/reference/cstring/)