---
title:                "Écrire sur l'erreur standard"
aliases:
- /fr/cpp/writing-to-standard-error.md
date:                  2024-02-03T19:32:32.326669-07:00
model:                 gpt-4-0125-preview
simple_title:         "Écrire sur l'erreur standard"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/cpp/writing-to-standard-error.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Quoi & Pourquoi ?

Écrire sur l'erreur standard (`stderr`) en C++ implique de sortir les messages d'erreur ou les diagnostics qui sont séparés de la sortie principale du programme. Les programmeurs font cela pour diriger les erreurs vers un flux différent, permettant ainsi un débogage et une gestion des erreurs plus aisés en distinguant la sortie normale des messages d'erreur.

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
