---
title:                "Verifica se una directory esiste"
aliases: - /it/cpp/checking-if-a-directory-exists.md
date:                  2024-02-03T19:06:52.263272-07:00
model:                 gpt-4-0125-preview
simple_title:         "Verifica se una directory esiste"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/cpp/checking-if-a-directory-exists.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Cosa e Perché?
Verificare l'esistenza di una directory significa determinare la presenza di una directory in un percorso specificato prima di eseguire operazioni come leggere o scrivere file al suo interno. I programmatori lo fanno per evitare errori legati alle operazioni sui file, garantendo un'esecuzione più fluida e affidabile dei compiti di gestione dei file nelle loro applicazioni.

## Come fare:
Nel C++ moderno (C++17 e versioni successive), puoi utilizzare la libreria filesystem per verificare se una directory esiste. Fornisce un modo diretto e standardizzato per eseguire operazioni sul filesystem, inclusa la verifica dell'esistenza di una directory.

```cpp
#include <iostream>
#include <filesystem>

namespace fs = std::filesystem;

int main() {
    const fs::path dirPath = "/path/to/directory";

    if (fs::exists(dirPath) && fs::is_directory(dirPath)) {
        std::cout << "La directory esiste." << std::endl;
    } else {
        std::cout << "La directory non esiste." << std::endl;
    }

    return 0;
}
```
Output di esempio se la directory esiste:
```
La directory esiste.
```

Output di esempio se la directory non esiste:
```
La directory non esiste.
```

Per progetti che non stanno ancora utilizzando C++17 o per caratteristiche aggiuntive, la libreria Boost Filesystem è una scelta di terze parti popolare che offre funzionalità simili.

```cpp
#include <iostream>
#include <boost/filesystem.hpp>

namespace fs = boost::filesystem;

int main() {
    const fs::path dirPath = "/path/to/directory";

    if (fs::exists(dirPath) && fs::is_directory(dirPath)) {
        std::cout << "La directory esiste." << std::endl;
    } else {
        std::cout << "La directory non esiste." << std::endl;
    }

    return 0;
}
```
Usando Boost Filesystem, l'output sarebbe identico all'esempio del filesystem C++17, a seconda dell'esistenza della directory nel percorso specificato.
