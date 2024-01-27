---
title:                "Verifica dell'esistenza di una directory"
date:                  2024-01-19
html_title:           "Arduino: Verifica dell'esistenza di una directory"
simple_title:         "Verifica dell'esistenza di una directory"
programming_language: "C++"
category:             "C++"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/cpp/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## What & Why?
Verificare se una directory esiste ci permette di evitare errori durante l'accesso o la modifica di file. I programmatori lo fanno per garantire che le operazioni sui file siano sicure e corrette.

## How to:
In C++17, usiamo `<filesystem>` per lavorare con i file system. Ecco come verificare l'esistenza di una directory:

```C++
#include <iostream>
#include <filesystem>

int main() {
    const std::filesystem::path dir{"./esempio"};

    if(std::filesystem::exists(dir)) {
        std::cout << "La directory esiste." << std::endl;
    } else {
        std::cout << "La directory non esiste." << std::endl;
    }
    
    return 0;
}
```

Output possibile:
`La directory esiste.`
o
`La directory non esiste.`

## Deep Dive:
Prima di C++17, controllare l'esistenza di una directory era più difficile. Si usavano funzioni di sistema come `stat` in POSIX o `GetFileAttributes` in Windows API. Queste funzioni non erano portabili e il codice diventava complesso.

Con l'introduzione del modulo `<filesystem>` in C++17, abbiamo un'interfaccia unificata e portabile. Oltre a `exists()`, ci sono funzioni come `is_directory()` e `create_directory()` che forniscono un controllo più granulare sull'interazione con i filesystem.

Inoltre, `std::filesystem::exists` può lanciare un'eccezione `std::filesystem::filesystem_error` se si verifica un errore di accesso alla directory, ad esempio se i permessi non sono sufficienti.

Alternativa storica, ancora utilizzata in codice non moderno o per motivi di compatibilità, include l'uso della funzione `opendir()` dalla libreria C. Ma `opendir()` non è standard C++ e richiede condizionali `#ifdef` per garantire la portabilità tra diversi sistemi operativi.

## See Also:
- C++ Reference for <filesystem>: https://en.cppreference.com/w/cpp/filesystem
- Boost Filesystem Library (una buona alternativa pre-C++17): https://www.boost.org/doc/libs/release/libs/filesystem/
- POSIX `stat` function: https://pubs.opengroup.org/onlinepubs/9699919799/functions/stat.html
- Microsoft Docs for `GetFileAttributes`: https://docs.microsoft.com/en-us/windows/win32/api/fileapi/nf-fileapi-getfileattributes
