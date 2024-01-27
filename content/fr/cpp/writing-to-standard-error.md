---
title:                "Écrire dans l'erreur standard"
date:                  2024-01-19
html_title:           "Arduino: Écrire dans l'erreur standard"
simple_title:         "Écrire dans l'erreur standard"
programming_language: "C++"
category:             "C++"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/cpp/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## What & Why?
Écrire dans la sortie d'erreur standard (stderr) permet d'envoyer des messages d'erreur sans les mélanger aux données de sortie normales. Les programmeurs l'utilisent pour diagnostiquer les problèmes en séparant le flux d'erreur du flux de sortie principal.

## How to:
Le C++ permet d'écrire directement dans `std::cerr`, qui est automatiquement associé à stderr.

```cpp
#include <iostream>

int main() {
    std::cerr << "Erreur: Fichier non trouvé." << std::endl;
    return 0;
}
```

Sortie typique en console:
```
Erreur: Fichier non trouvé.
```

## Deep Dive
Avant C++, le flux stderr existait déjà en C, utilisant `fprintf(stderr, ...)`. En C++, `std::cerr` est préférable pour la surcharge d'opérateurs et la sécurité de type. Il est non bufferisé par défaut, donc les messages sont affichés immédiatement, important pour les erreurs. Par contre, `std::clog` est bufferisé et peut aussi être utilisé pour des logs d'erreurs différés.

## See Also
- Tutorials on C++ IOStreams: [http://www.cplusplus.com/reference/iostream/](http://www.cplusplus.com/reference/iostream/)
