---
title:    "C: Concatenazione di stringhe"
keywords: ["C"]
---

{{< edit_this_page >}}

## Perché

La concatenazione di stringhe è un'operazione comune nel linguaggio di programmazione C che viene utilizzata per combinare due o più stringhe in una nuova stringa più lunga. Questo può essere utile per creare messaggi di output personalizzati o per manipolare i dati in modo più efficiente.

## Come fare

Per concatenare le stringhe in C, è necessario utilizzare la funzione `strcat()` che è definita nella libreria di stringhe `string.h`. Questa funzione prende due argomenti: una stringa di destinazione e una stringa di origine. La stringa di origine verrà aggiunta alla fine della stringa di destinazione, creando una nuova stringa concatenata.

Di seguito è riportato un esempio di codice che combina due stringhe e stampa il risultato:

``` C
#include <stdio.h>
#include <string.h>

int main(void) {
    char stringa1[20] = "Ciao, ";
    char stringa2[] = "mondo!";

    strcat(stringa1, stringa2);

    printf("%s", stringa1);

    return 0;
}
```

Il risultato di questo codice sarà:

```
Ciao, mondo!
```

È importante notare che la stringa di destinazione deve avere una dimensione sufficientemente grande per contenere la stringa concatenata. In caso contrario, si potrebbe verificare un errore durante l'esecuzione del programma.

## Approfondimento

La funzione `strcat()` racchiude in realtà molte operazioni sotto il cofano. Utilizza il puntatore alla fine della stringa di destinazione per determinare dove aggiungere la nuova stringa, quindi copia i caratteri della stringa di origine in quella posizione. Questo processo continua fino a quando non viene raggiunto il carattere terminatore `'\0'`, che indica la fine della stringa.

Inoltre, è importante notare che la funzione `strcat()` aggiunge sempre la stringa di origine alla fine della stringa di destinazione e non può essere utilizzata per inserire una stringa tra due parti di una stringa.

## Vedi anche

- [La funzione `strcat()` su cplusplus.com](http://www.cplusplus.com/reference/cstring/strcat/)
- [Tutorial sulla manipolazione di stringhe in C](https://www.tutorialspoint.com/cprogramming/c_strings.htm)
- [Esempi pratici di concatenazione di stringhe in C](https://www.programiz.com/c-programming/examples/concatenate-strings)

Grazie per aver letto questo breve articolo sulla concatenazione di stringhe in C! Spero ti sia stato utile e ti aiuti a comprendere meglio questa importante parte del linguaggio di programmazione. Continua a esplorare le possibilità di C e buon coding!