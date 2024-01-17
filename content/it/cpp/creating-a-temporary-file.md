---
title:                "Creazione di un file temporaneo"
html_title:           "C++: Creazione di un file temporaneo"
simple_title:         "Creazione di un file temporaneo"
programming_language: "C++"
category:             "C++"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/cpp/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Cosa e Perché?

Creare un file temporaneo è un processo comune per i programmatori. Si tratta di creare un file che viene utilizzato temporaneamente durante l'esecuzione di un programma e che viene poi eliminato una volta che il programma è terminato. I programmatori usano i file temporanei per una varietà di motivi, tra cui il salvataggio dei dati temporanei, il testing di codici o il gestire file di grandi dimensioni.

## Come fare:

```C++
#include <iostream>
#include <fstream>
#include <cstdlib>
using namespace std;

int main()
{
    // Creazione di un file temporaneo utilizzando la funzione tmpfile()
    FILE *tempFile = tmpfile();
    if (tempFile == NULL)
    {
        cout << "Errore nella creazione del file temporaneo!" << endl;
        exit(1);
    }

    // Scrittura di dati all'interno del file temporaneo
    fprintf(tempFile, "Questo è un file temporaneo.");

    // Chiusura del file temporaneo
    fclose(tempFile);
    remove(tempFile);

    return 0;
}
```
**Output:**
```
Questo è un file temporaneo.
```

## Approfondimento:

**Contesto storico:** I file temporanei sono stati introdotti in ambito informatico nel 1970 dal linguaggio di programmazione C. In passato, i programmatori dovevano creare manualmente i file e poi eliminarli dopo l'uso, mentre oggi, grazie alla funzione tmpfile(), il processo è automatizzato.

**Alternative:** Oltre alla funzione tmpfile(), i programmatori possono anche utilizzare altre funzioni come tmpnam() e mkstemp() per creare file temporanei. Inoltre, alcune librerie esterne come Boost offrono metodi per gestire i file temporanei.

**Dettagli di implementazione:** La funzione tmpfile() crea un file temporaneo e lo apre per la scrittura in modalità binaria. Il file viene creato nella directory temporanea del sistema operativo e viene generato un nome univoco per il file. Una volta che il file viene chiuso o il programma termina, il file viene automaticamente eliminato dal sistema operativo.

## Vedi anche:

- [Documentazione di tmpfile() su cplusplus.com](https://www.cplusplus.com/reference/cstdio/tmpfile/)
- [Come utilizzare i file temporanei in C++](https://www.tutorialspoint.com/cplusplus/cpp_files_streams.html)