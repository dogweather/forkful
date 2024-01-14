---
title:    "C: Creare un file temporaneo."
keywords: ["C"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/it/c/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Perché

Ci sono molte ragioni per voler creare un file temporaneo in un programma C. Un caso comune è quando si lavora con file di grandi dimensioni e si vuole salvare temporaneamente alcune parti del file senza doverlo modificare direttamente.

## Come Fare

Per creare un file temporaneo in C, abbiamo bisogno di utilizzare la funzione `tmpfile()`. Questa funzione crea un file temporaneo vuoto e restituisce un puntatore a quel file. Vediamo un esempio di codice:

```c
#include <stdio.h>
#include <stdlib.h>

int main() {
    FILE *fp = tmpfile(); //crea il file temporaneo
    if (!fp) {
        printf("Errore nella creazione del file temporaneo!\n");
        return 1;
    }
    fputs("Questo è un file temporaneo!", fp); //scrive nel file temporaneo
    fclose(fp); //chiude il file temporaneo

    printf("File temporaneo creato con successo!\n");
    return 0;
}
```

Se eseguiamo il codice sopra, otterremo un output simile a questo:

```
File temporaneo creato con successo!
```

## Approfondimento

Oltre alla funzione `tmpfile()`, ci sono anche altre funzioni che possono essere utili quando si lavora con file temporanei. Ad esempio, la funzione `tmpnam()` viene utilizzata per generare un nome casuale per il file temporaneo, mentre `tmpnam_r()` restituisce il nome del file temporaneo in una stringa passata come argomento.

Un'importante cosa da notare è che i file temporanei creati con `tmpfile()` vengono cancellati automaticamente quando il programma termina o quando vengono chiusi con `fclose()`. Inoltre, questi file non sono accessibili da altri processi in esecuzione sul sistema.

## Vedi Anche

- [Documentazione di tmpfile()](https://www.tutorialspoint.com/c_standard_library/c_function_tmpfile.htm)
- [Guida all'utilizzo dei file temporanei in C](https://www.programiz.com/c-programming/c-file-input-output)