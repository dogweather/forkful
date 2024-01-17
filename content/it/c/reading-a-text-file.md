---
title:                "Lettura di un file di testo"
html_title:           "C: Lettura di un file di testo"
simple_title:         "Lettura di un file di testo"
programming_language: "C"
category:             "C"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/c/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Cosa & Perché?
Leggere un file di testo è semplicemente il processo di aprire e leggere un file di testo presente sul computer. I programmatori lo fanno principalmente per accedere a dati strutturati in un formato comune e utilizzarli nel proprio codice.

## Come fare:
Una volta aperto il file, è possibile utilizzare la funzione `fscanf` per leggere i dati uno alla volta e salvare i valori in variabili o array. Ecco un esempio:

```C
FILE *file = fopen("dati.txt", "r"); // "r" indica che il file è aperto in modalità lettura
if (file == NULL) {
    printf("Impossibile aprire il file.");
    return -1;
}

int numero;
char carattere;

// Leggi un numero intero e un carattere dalla prima riga del file
fscanf(file, "%d %c", &numero, &carattere);

// Ora è possibile utilizzare le variabili "numero" e "carattere" nel proprio codice

fclose(file); // Chiude il file per evitare perdite di dati
```

Il contenuto del file "dati.txt" potrebbe essere ad esempio:

```
10 A
```
e il risultato dell'esempio sarebbe:

```
numero = 10
carattere = 'A'
```

## Approfondimento:
Leggere un file di testo era una delle prime funzioni disponibili nelle prime versioni del linguaggio C, risalenti agli anni '70. Oggigiorno, esistono anche altre modalità di lettura dei dati, come ad esempio l'utilizzo di API per accedere a database o l'utilizzo di protocolli di comunicazione come HTTP.

## Vedi anche:
- La documentazione ufficiale di C per la funzione `fscanf`: https://www.cplusplus.com/reference/cstdio/fscanf/
- Un esempio di lettura di un file di testo in C++: https://www.geeksforgeeks.org/read-write-file-c/