---
title:    "C++: Verifica se una directory esiste"
keywords: ["C++"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/it/cpp/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Perché Controllare se una Directory Esiste?

Controllare se una directory esiste è un'operazione comune nella programmazione di C++. Ciò può essere necessario per garantire che un programma funzioni correttamente o per effettuare operazioni specifiche su una directory esistente. In questa guida, impareremo come controllare se una directory esiste utilizzando il linguaggio C++.

## Come Fare

Iniziamo con l'include delle librerie necessarie per il nostro programma:

``` C++
#include <iostream>
#include <filesystem>
```

Successivamente, utilizzeremo la funzione "exists" della libreria "filesystem" per controllare se una determinata directory esiste e stampare un messaggio di output di conseguenza:

``` C++
if (std::filesystem::exists("directory"))
    std::cout << "La directory esiste!" << std::endl;
else
    std::cout << "La directory non esiste!" << std::endl;
```

In questo esempio, stiamo controllando se la directory chiamata "directory" esiste. Nel caso in cui non esista, verrà stampato il messaggio "La directory non esiste!".

## Approfondimento

Oltre alla funzione "exists", la libreria "filesystem" offre anche altre opzioni per lavorare con directory. Ad esempio, la funzione "create_directory" permette di creare una nuova directory e la funzione "remove" permette di eliminare una directory esistente. Inoltre, con la funzione "current_path" è possibile ottenere il percorso della directory corrente.

Per ulteriori informazioni su come lavorare con le directory utilizzando la libreria "filesystem", si consiglia di consultare la documentazione ufficiale di C++.

## Vedi Anche

- Documentazione ufficiale di C++ sulla libreria "filesystem": https://en.cppreference.com/w/cpp/filesystem
- Tutorial su come lavorare con file e directory in C++: https://www.learncpp.com/cpp-tutorial/introduction-to-the-file-system/