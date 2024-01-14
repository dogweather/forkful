---
title:    "C++: Verifica dell'esistenza di una directory"
keywords: ["C++"]
---

{{< edit_this_page >}}

## Perché

Una delle attività comuni nel processo di sviluppo di un programma è quella di dover verificare l'esistenza di una directory. Questo può essere necessario per molteplici motivi, come ad esempio accedere a file o cartelle specifici o per garantire l'integrità e la sicurezza del programma stesso. In questo post, vedremo come verificare la presenza di una directory utilizzando il linguaggio di programmazione C++.

## Come fare

Per verificare se una directory esiste, possiamo utilizzare la funzione `opendir()` che fa parte della libreria standard di C++. Questa funzione prende in input il percorso della directory che vogliamo verificare e restituisce un puntatore a un oggetto di tipo `DIR`. Se la directory non esiste, la funzione restituirà un valore nullo.

Di seguito mostriamo un esempio di come utilizzare la funzione `opendir()` per verificare l'esistenza di una directory chiamata "nuovaCartella":

```C++
#include <iostream>
#include <dirent.h>

int main() {
    DIR *dir = opendir("nuovaCartella"); // apriamo la directory
   
    if (dir == NULL) { // se la directory non esiste
        std::cout << "La directory non esiste." << std::endl;
    } else { // se la directory esiste
        std::cout << "La directory esiste." << std::endl;
        closedir(dir); // chiudiamo la directory
    }
    
    return 0;
}
```

Ecco un possibile output del programma:

```
La directory esiste.
```

## Approfondimento

Esistono anche altre funzioni che possono essere utilizzate per verificare l'esistenza di una directory, come ad esempio `stat()` e `access()`. Inoltre, è importante tenere presente che anche se una directory esiste, potrebbe non essere possibile accedervi a causa dei permessi di accesso impostati. È quindi necessario gestire questo tipo di situazioni per garantire il corretto funzionamento del programma.

## Vedi anche

- [Funzione opendir()](https://www.cplusplus.com/reference/cstdio/fopen/)
- [Funzione stat()](https://www.cplusplus.com/reference/cstdio/stat/)
- [Funzione access()](https://www.cplusplus.com/reference/cunistd/access/)