---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:32:32.326669-07:00
description: "\xC9crire sur l'erreur standard (`stderr`) en C++ implique de sortir\
  \ les messages d'erreur ou les diagnostics qui sont s\xE9par\xE9s de la sortie principale\
  \ du\u2026"
lastmod: '2024-03-13T22:44:58.183657-06:00'
model: gpt-4-0125-preview
summary: "\xC9crire sur l'erreur standard (`stderr`) en C++ implique de sortir les\
  \ messages d'erreur ou les diagnostics qui sont s\xE9par\xE9s de la sortie principale\
  \ du programme."
title: "\xC9crire sur l'erreur standard"
weight: 25
---

## Comment faire :
En C++, l'écriture sur l'erreur standard peut être réalisée en utilisant le flux `cerr`, qui fait partie de la bibliothèque standard. Voici un exemple basique :

```cpp
#include <iostream>

int main() {
    // Écriture sur la sortie standard
    std::cout << "Ceci est un message normal." << std::endl;
    
    // Écriture sur l'erreur standard
    std::cerr << "Ceci est un message d'erreur." << std::endl;
    
    return 0;
}
```

Exemple de sortie :
```
Ceci est un message normal.
Ceci est un message d'erreur.
```

Dans ce cas, les deux messages apparaîtront typiquement sur votre terminal, mais vous pouvez les rediriger séparément dans un shell. Par exemple, vous pouvez envoyer la sortie standard vers un fichier tout en permettant aux erreurs d'être affichées à l'écran.

Pour une journalisation et une gestion des erreurs plus avancées, des bibliothèques tierces comme `spdlog` ou `boost.log` peuvent être employées. Ces bibliothèques offrent des fonctionnalités améliorées pour la journalisation, incluant la mise en format, les niveaux de log et la sortie fichier.

Voici comment vous pourriez utiliser `spdlog` pour écrire un message d'erreur :

```cpp
#include "spdlog/spdlog.h"

int main() {
    // Initialisation de spdlog
    spdlog::info("Ceci est un message normal.");
    spdlog::error("Ceci est un message d'erreur.");
    
    return 0;
}
```

Note : Pour utiliser `spdlog`, vous devez l'ajouter à votre projet. Vous pouvez le faire en clonant le dépôt depuis GitHub ou en utilisant un gestionnaire de paquets comme `vcpkg` ou `conan`.

Rappelez-vous, le choix entre utiliser directement les flux standards ou une bibliothèque comme `spdlog` dépend de la complexité de votre application et de vos besoins spécifiques concernant la gestion des erreurs et la journalisation.
