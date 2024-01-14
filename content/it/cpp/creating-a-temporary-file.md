---
title:    "C++: Creazione di un file temporaneo"
keywords: ["C++"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/it/cpp/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Perché

Creare un file temporaneo è spesso necessario durante la programmazione, ad esempio per salvare dati temporanei o per eseguire operazioni su un file senza alterare l'originale.

## Come fare

Per creare un file temporaneo in C++, è necessario includere la libreria "fstream" che ci permette di gestire i file. Utilizziamo la funzione "tmpfile()" per creare il file e assegnarne un puntatore.

```C++
#include <fstream>
...
// Creiamo il file temporaneo
FILE *tempFile = tmpfile();
```

A questo punto, possiamo scrivere all'interno del file utilizzando la funzione "fprintf()":

```C++
// Scriviamo una stringa all'interno del file
fprintf(tempFile, "Questo è uno stringa di esempio");
```

Infine, per leggere il contenuto del file temporaneo, utilizziamo la funzione "fscanf()":

```C++
char str[50];
// Leggiamo la stringa dal file temporaneo
fscanf(tempFile, "%s", str);
// Stampiamo il risultato
cout << str << endl; // Output: Questo è uno stringa di esempio
```

## Approfondimento

La funzione "tmpfile()" crea un file temporaneo in modo sicuro, in modo da evitare conflitti con altri processi che possono utilizzare lo stesso nome di file temporaneo. Inoltre, il file viene eliminato automaticamente quando viene chiuso il programma. È importante notare che il file temporaneo esiste solo durante l'esecuzione del programma e non viene salvato sul disco fisso.

## Vedi anche

- [Documentazione della funzione tmpfile() in C++](https://en.cppreference.com/w/c/io/tmpfile)
- [Tutorial su come gestire i file in C++](https://www.geeksforgeeks.org/file-handling-c-classes/)
- [Esempi pratici di creazione di file temporanei in C++](https://www.cplusplus.com/forum/beginner/237871/)