---
title:                "C: Leggere un file di testo"
simple_title:         "Leggere un file di testo"
programming_language: "C"
category:             "C"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/c/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Perché

Leggere un file di testo può sembrare una semplice operazione, ma è una delle abilità fondamentali per qualsiasi programmatore C. Grazie a questa tecnica, è possibile manipolare e utilizzare dati salvati esternamente, aprendo una vasta gamma di possibilità di programmazione.

## Come fare

Per leggere un file di testo in C, è necessario utilizzare la funzione `fopen()` per aprire il file e ottenere un puntatore a esso. Successivamente, si può utilizzare la funzione `fgets()` per leggere una riga alla volta dal file e la funzione `fscanf()` per leggere dati formattati. Ecco un esempio di codice per leggere un file di testo e stamparne il contenuto:

```C
FILE *fp;
char line[50];

fp = fopen("file.txt", "r");

if (fp == NULL) {
    printf("Errore nell'apertura del file!");
    return 0;
}

while (fgets(line, sizeof(line), fp) != NULL) {
    printf("%s\n", line);
}

fclose(fp);
```

L'output di questo esempio sarà il contenuto del file `file.txt`, stampato riga per riga.

## Approfondimento

Esistono diverse opzioni per la lettura di un file di testo in C. Ad esempio, la funzione `fgetc()` permette di leggere carattere per carattere, mentre `getc()` è simile a `fgetc()` ma può essere utilizzata con un file gestito da una libreria esterna. Inoltre, è possibile utilizzare `fscanf()` per leggere dati formattati come interi o float, utilizzando la stessa sintassi della funzione `scanf()`.

È importante anche gestire eventuali errori durante la lettura del file, ad esempio verificando se il puntatore ottenuto da `fopen()` è nullo (ovvero se ci sono stati problemi durante l'apertura) e chiudere il file con `fclose()` una volta finito di utilizzarlo. Inoltre, è buona prassi utilizzare una variabile per memorizzare la lunghezza massima delle righe che si desidera leggere, in modo da evitare di superare la dimensione del buffer utilizzato per l'array `line` nel nostro esempio.

## Vedi anche (See Also)

- [Documentazione ufficiale di fopen()](https://www.gnu.org/software/libc/manual/html_node/Opening-and-Closing-Files.html)
- [Esempi di lettura di file di testo in C](https://www.programiz.com/c-programming/c-file-input-output)
- [Differenze tra le funzioni fgetc() e getc()](https://stackoverflow.com/questions/19361114/c-reading-a-text-file-using-only-getc)
- [Tutorial sulle librerie esterne in C](https://www.tutorialspoint.com/compiling-cc-programs-using-external-libraries)