---
title:                "C++: Verifica dell'esistenza di una directory"
simple_title:         "Verifica dell'esistenza di una directory"
programming_language: "C++"
category:             "C++"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/cpp/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Perché
Quando si scrive un programma, è importante controllare se una determinata directory esiste prima di accedere ai suoi file o sottodirectory. In questo modo si prevengono errori e crash improvvisi del programma.

## Come fare
Ecco un esempio di come verificare se una directory esiste in C++ utilizzando la funzione `std::filesystem::exists()`:

```C++
#include <iostream>
#include <filesystem>

int main() {
  std::filesystem::path directory_path = "/home/user/documents";
  if(std::filesystem::exists(directory_path)) {
    std::cout << "La directory esiste!" << std::endl;
  } else {
    std::cout << "La directory non esiste!" << std::endl;
  }
  return 0;
}
```

Output:

```
La directory esiste!
```

## Approfondimento
Per verificare se una directory esiste in modo più dettagliato, ci sono alcune cose che è importante sapere. Innanzitutto, la funzione `std::filesystem::exists()` restituisce un valore booleano, `true` se la directory esiste e `false` se non esiste. Inoltre, è possibile specificare una serie di criteri opzionali nella funzione per cercare la directory. Ad esempio, si può specificare se si desidera cercare solo una directory o un file con lo stesso nome, o se si vuole cercare all'interno di una determinata directory o in tutte le sottodirectory. 

Inoltre, è importante notare che la funzione `std::filesystem::exists()` dipende da come è implementato il file system del sistema operativo. Ad esempio, alcune piattaforme potrebbero non supportare la verifica di esistenza di file o directory con nomi di file non ASCII.

## Vedi anche
- [Documentazione della funzione `std::filesystem::exists()`](https://en.cppreference.com/w/cpp/filesystem/exists)
- [Tutorial su come gestire i file e le directory in C++](https://www.learncpp.com/cpp-tutorial/working-with-files/)
- [Esempi di codice per la gestione delle directory in C++](https://www.geeksforgeeks.org/working-with-directories-in-c-cpp/)