---
title:                "Verifica se una directory esiste"
html_title:           "C++: Verifica se una directory esiste"
simple_title:         "Verifica se una directory esiste"
programming_language: "C++"
category:             "C++"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/cpp/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

# Cosa & Perché?
La verifica dell'esistenza di una directory è un'operazione comune nei programmatori che utilizzano il linguaggio C++. Ciò è necessario per garantire che un percorso di file specificato sia valido prima di eseguire operazioni come scrittura o lettura da esso.

# Come: 
Di seguito sono riportati due esempi di codice in C++ per verificare se una directory esiste utilizzando la libreria standard di C++11.

```
#include <iostream>
#include <experimental/filesystem>

namespace fs = std::experimental::filesystem;

int main() {
    // Metodo 1: utilizzando la funzione exists()
    if (fs::exists("directory/")) {
        std::cout << "La directory esiste!" << std::endl;
    } else {
        std::cout << "La directory non esiste." << std::endl;
    }

    // Metodo 2: utilizzando la funzione is_directory()
    if (fs::is_directory("directory/")) {
        std::cout << "La directory esiste!" << std::endl;
    } else {
        std::cout << "La directory non esiste." << std::endl;
    }

    return 0;
}
```

**Output:**
```
La directory esiste!
La directory esiste!
```

# Profondità: 
In passato, la libreria Boost.Filesystem era l'unico modo per verificare l'esistenza di una directory in C++. Tuttavia, con l'introduzione della libreria standard C++11, la funzionalità di gestione dei file è stata aggiunta e semplificata.

Un'alternativa alla libreria standard C++11 è la libreria Open Asset Import (Assimp), che offre funzionalità avanzate di gestione dei file, inclusa la verifica dell'esistenza di una directory.

Per implementare il controllo dell'esistenza di una directory, la libreria standard C++11 utilizza una combinazione di funzioni di sistema e chiamate al sistema operativo per ottenere informazioni sul percorso specificato.

# Vedi anche:
- [Documentazione sulla libreria standard C++11](https://en.cppreference.com/w/cpp/filesystem)
- [Documentazione sulla libreria Open Asset Import (Assimp)](http://www.assimp.org/)
- [Documentazione sulla libreria Boost.Filesystem](https://www.boost.org/doc/libs/1_75_0/libs/filesystem/doc/index.htm)