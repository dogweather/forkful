---
title:    "C: Creare un file temporaneo"
keywords: ["C"]
---

{{< edit_this_page >}}

## Perché

Creare file temporanei è un'operazione fondamentale per molti programmatori in C. I file temporanei consentono di memorizzare temporaneamente dati all'interno del programma e possono essere utilizzati per una varietà di scopi, come il caching dei risultati di calcoli o la memorizzazione di dati sensibili in modo sicuro.

## Come fare

Per creare un file temporaneo in C, è necessario utilizzare la libreria standard `stdio.h` e la funzione `tmpfile()`. Questa funzione creerà un file temporaneo in modalità di scrittura e restituirà un puntatore `FILE` al file creato.

```C
#include <stdio.h>

int main()
{
    FILE *temp_file = tmpfile();
    if (temp_file == NULL)
    {
        printf("Errore nella creazione del file temporaneo");
        return 1;
    }

    // Codice per scrivere dati nel file temporaneo
    
    fclose(temp_file); // Chiude il file temporaneo
}
```

Una volta creato il file temporaneo, è possibile utilizzarlo come qualsiasi altro file in modalità di scrittura. È possibile utilizzare le funzioni `fprintf()` o `fwrite()` per scrivere dati all'interno del file.

```C
fprintf(temp_file, "Il valore di x è %d", x); // Scrive una stringa di testo nel file
fwrite(array, sizeof(int), num_elements, temp_file); // Scrive un array di interi nel file
```

Inoltre, per leggere i dati dal file temporaneo, è possibile utilizzare le funzioni `fscanf()` o `fread()`.

```C
int value;
fscanf(temp_file, "%d", &value); // Legge un intero dal file
fread(array, sizeof(int), num_elements, temp_file); // Legge un array di interi dal file
```

Una volta terminato l'utilizzo del file temporaneo, è necessario chiuderlo utilizzando la funzione `fclose()`. Ciò garantisce che il file venga eliminato dal sistema operativo.

## Approfondimento

È importante notare che i file temporanei vengono creati nella directory temporanea del sistema operativo. Ciò significa che quando il programma termina o il file viene chiuso, verrà automaticamente eliminato dal sistema. Questo è particolarmente utile per evitare l'accumulo di file inutili e risparmiare spazio nel disco fisso.

Inoltre, è possibile specificare un percorso diverso per la creazione del file temporaneo utilizzando la funzione `tmpfile()` in combinazione con la funzione `tmpnam()`. Questa funzione restituirà un nome univoco per il file temporaneo nella directory specificata.

```C
#include <stdio.h>

int main()
{
    char temp_path[L_tmpnam];
    tmpnam(temp_path); // Genera un nome univoco per il file temporaneo
    FILE *temp_file = tmpfile(temp_path); // Crea il file temporaneo nella directory specificata
}
```

## Vedi anche

- [Documentazione su `tmpfile()`](https://www.gnu.org/software/libc/manual/html_node/Temporary-Files.html)
- [Esempi sull'utilizzo di file temporanei in C](https://www.geeksforgeeks.org/temporary-file-creation-using-c-programming/)
- [Tutorial sulla gestione dei file in C](https://www.learn-c.org/en/Files)