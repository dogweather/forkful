---
title:                "Verifica dell'esistenza di una directory"
html_title:           "C++: Verifica dell'esistenza di una directory"
simple_title:         "Verifica dell'esistenza di una directory"
programming_language: "C++"
category:             "C++"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/cpp/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Perché
Spesso, mentre scriviamo codice, è necessario controllare se una determinata directory esiste o meno. Questo ci aiuta a gestire il flusso del programma e ad evitare errori durante la lettura o la scrittura di file.

## Come Fare
Controllare se una directory esiste in C++ è abbastanza semplice. Utilizzando la libreria `<filesystem>`, possiamo utilizzare la funzione `std::filesystem::exists()` per determinare se la directory esiste o meno. Di seguito un esempio di codice con una directory esistente ed una inesistente:

```C++
#include <iostream>
#include <filesystem>

int main()
{
    // Controllo della directory esistente
    if (std::filesystem::exists("Documents"))
    {
        std::cout << "La directory esiste!" << std::endl;
    }
    
    // Controllo della directory inesistente
    if (std::filesystem::exists("Photos"))
    {
        std::cout << "La directory esiste!" << std::endl;
    } 
    else
    {
        std::cout << "La directory non esiste!" << std::endl;
    }

    return 0;
}
```

Ecco la corrispondente output:

```
La directory esiste!
La directory non esiste!
```

## Approfondimento
La funzione `std::filesystem::exists()` utilizza la classe `std::experimental::filesystem::file_status` per determinare se un particolare percorso esiste. Questa classe fornisce anche informazioni aggiuntive sul percorso, come ad esempio i permessi di accesso e la data di ultima modifica. Per ulteriori informazioni su questa classe e altre funzioni utili per gestire i file e le directory in C++, si consiglia di consultare la documentazione ufficiale.

## Vedi Anche
- [Funzione `std::filesystem::exists()`: Documentazione C++](https://en.cppreference.com/w/cpp/filesystem/exists)
- [Libreria `<filesystem>`: Documentazione C++](https://en.cppreference.com/w/cpp/filesystem)
- [Gestione dei file e delle directory in C++: Tutorial su TutorialsPoint](https://www.tutorialspoint.com/cplusplus/cpp_files_streams.htm)