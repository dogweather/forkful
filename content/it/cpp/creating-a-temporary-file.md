---
title:                "C++: Creazione di un file temporaneo"
programming_language: "C++"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/cpp/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Perché

Creare un file temporaneo è uno strumento utile per la gestione dei dati all'interno di un programma. Può essere utilizzato per archiviare temporaneamente informazioni che verranno poi eliminate o per generare dati temporanei per l'utilizzo in altre funzioni del programma.

## Come fare

Per creare un file temporaneo in C++, è necessario utilizzare la libreria <fstream> e la funzione di sistema "tmpnam ()". Di seguito un esempio di codice:

```
#include <fstream>
#include <iostream>
#include <cstdlib>

using namespace std;

int main () {

  char nome_file[1024];

  // Utilizzo della funzione tmpnam () per creare il nome del file temporaneo
  tmpnam(nome_file);
  cout << "Il file temporaneo è: " << nome_file << endl;

  // Apertura del file temporaneo in modalità di scrittura
  ofstream file_temporaneo (nome_file);

  // Scrittura di dati all'interno del file
  file_temporaneo << "Questo è un file temporaneo generato dal programma C++" << endl;

  // Chiusura del file
  file_temporaneo.close();

  // Eliminazione del file temporaneo
  remove(nome_file);

  return 0;
}
```

Al termine dell'esecuzione del programma, verrà visualizzato il nome del file temporaneo creato. Successivamente il file viene aperto in modalità di scrittura e scritti al suo interno dei dati. Infine, il file viene chiuso e cancellato utilizzando la funzione "remove ()".

## Approfondimento

La funzione "tmpnam ()" restituisce un nome univoco per il file temporaneo creato, utilizzando la directory specificata dal sistema come percorso di default. Se si desidera specificare una directory diversa, è possibile utilizzare la funzione "tmpnam_r()", che richiede come parametro il percorso della directory desiderata.

Un altro modo per creare un file temporaneo è utilizzare la funzione "tmpfile ()". Questa funzione crea automaticamente un file temporaneo e restituisce un puntatore alla sua struttura FILE. In questo modo è possibile utilizzare le funzioni della libreria <stdio.h> per scrivere e leggere dati dal file temporaneo.

## Vedi anche

- [Documentazione sulla funzione tmpnam()](https://www.cplusplus.com/reference/cstdio/tmpnam/)
- [Documentazione sulla funzione tmpnam_r()](https://www.cplusplus.com/reference/cstdio/tmpnam_r/)
- [Documentazione sulla funzione tmpfile()](https://www.cplusplus.com/reference/cstdio/tmpfile/)