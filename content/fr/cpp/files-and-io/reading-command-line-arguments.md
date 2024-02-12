---
title:                "Lecture des arguments de ligne de commande"
aliases: - /fr/cpp/reading-command-line-arguments.md
date:                  2024-01-20T17:55:29.369164-07:00
model:                 gpt-4-1106-preview
simple_title:         "Lecture des arguments de ligne de commande"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/cpp/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## What & Why? (Quoi et Pourquoi ?)
Lire les arguments de ligne de commande permet à vos programmes C++ d’agir sur des infos fournies au lancement. On le fait pour rendre les applis flexibles et adaptatives sans les modifier.

## How to (Comment faire)
```C++
#include <iostream>

int main(int argc, char* argv[]) {
    std::cout << "Vous avez passé " << argc - 1 << " arguments:" << std::endl;
    for (int i = 1; i < argc; ++i) {
        std::cout << i << ": " << argv[i] << std::endl;
    }
    return 0;
}
```
Si vous lancez `./monprogramme Salut C++!`, l’output sera:
```
Vous avez passé 2 arguments:
1: Salut
2: C++!
```

## Deep Dive (Plongée Profonde)
Les arguments de ligne de commande existent depuis les premiers jours d'UNIX, offrant une interface simple pour contrôler les programmes. Des alternatives telles que les fichiers de configuration ou les entrées interactives utilisateur sont moins directes. En C++, `argc` indique le nombre d'arguments et `argv` est un tableau de chaînes contenant ces arguments. Le premier argument `argv[0]` est le nom du programme.

## See Also (Voir Aussi)
- La documentation officielle C++ sur `main()` : https://en.cppreference.com/w/cpp/language/main_function
- Tutoriels C++ sur les paramètres de ligne de commande: https://www.learncpp.com/cpp-tutorial/command-line-arguments/
