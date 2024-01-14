---
title:    "C: Lettura di un file di testo"
keywords: ["C"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/it/c/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Perché leggere un file di testo in C?

Leggere un file di testo è un'operazione comune nella programmazione, specialmente in C. Questo ti permette di accedere ai dati all'interno di un file e usarli all'interno del tuo programma. Se stai imparando il linguaggio C, è importante capire come eseguire questa operazione in modo efficace.

## Come fare per leggere un file di testo in C

Per leggere un file di testo in C, dovrai utilizzare le funzioni di input/output (I/O) della libreria standard. Queste funzioni ti permettono di aprire, leggere e chiudere un file di testo. Ecco un esempio di codice su come fare:

```c
// Dichiarazione di una variabile di tipo FILE
FILE *fp;
// Apertura del file utilizzando fopen()
fp = fopen("nome_file.txt", "r");
// Verifica se il file è stato aperto correttamente
if (fp == NULL) {
    printf("Errore nell'apertura del file");
    return 1;
}
// Leggi il contenuto del file utilizzando fscanf()
// Esempio: leggi una stringa e stampala a schermo
char stringa[100];
fscanf(fp, "%s", stringa);
printf("Il contenuto del file è: %s", stringa);
// Chiudi il file utilizzando fclose()
fclose(fp);
```
L'esempio sopra apre il file "nome_file.txt" in modalità di sola lettura ("r") utilizzando la funzione `fopen()` e verifica se è stato aperto correttamente. Successivamente, utilizza la funzione `fscanf()` per leggere una stringa dal file e la stampa a schermo utilizzando la funzione `printf()`. Infine, il file viene chiuso utilizzando `fclose()`.

## Approfondimenti sulla lettura di un file di testo in C

Nella sezione precedente, abbiamo visto come aprire e leggere un file di testo, ma ci sono altre funzioni utili che puoi utilizzare per manipolare i dati all'interno del file. Ad esempio, la funzione `fgets()` ti permette di leggere una riga intera dal file, mentre la funzione `fprintf()` ti permette di scrivere dati all'interno del file.

Inoltre, è importante prestare attenzione alla modalità di apertura del file. Nel nostro esempio precedente, abbiamo aperto il file in modalità di sola lettura. Se invece desideri scriverci dei dati, dovrai aprirlo in modalità di scrittura ("w"). Ci sono anche altre modalità disponibili, come ad esempio la modalità di lettura e scrittura ("r+").

## Vedi anche

- [Documentazione delle funzioni di input/output in C](https://www.tutorialspoint.com/c_standard_library/c_function_fopen.htm)
- [Tutorial sulla gestione dei file in C](https://www.programiz.com/c-programming/c-file-input-output)