---
title:                "Utilisation d'une console interactive (REPL)"
date:                  2024-01-26T04:12:07.822909-07:00
model:                 gpt-4-0125-preview
simple_title:         "Utilisation d'une console interactive (REPL)"

tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/cpp/using-an-interactive-shell-repl.md"
---

{{< edit_this_page >}}

## Quoi & Pourquoi ?
Un REPL (Read-Eval-Print-Loop, ou Boucle de Lecture, Évaluation et Impression en Français) est un environnement de programmation simple et interactif. Les programmeurs l'utilisent pour des expérimentations en temps réel avec le langage, des tâches rapides, ou pour comprendre de nouveaux concepts sans la charge de créer des applications complètes.

## Comment faire :
C++ ne vient pas avec un REPL intégré, mais des outils comme Cling offrent cette capacité. Voici comment utiliser Cling pour calculer la somme de deux nombres :

```C++
#include <iostream>

int main() {
    int a = 5;
    int b = 7;
    std::cout << "La somme est : " << a + b << std::endl;
    return 0;
}

// Sortie :
// La somme est : 12
```

Démarrez Cling et entrez le code ligne par ligne, observant la sortie après chaque commande. C'est un retour immédiat, sans compilation.

## Plongée profonde
Les REPL sont courants pour des langages comme Python ou Lisp, et existent depuis les années 1960. Pour C++, un langage compilé, le concept ne s'adapte pas aussi naturellement, c'est pourquoi des outils comme Cling existent, ils interprètent C++ à la volée. Les alternatives incluent les compilateurs en ligne ou de petits programmes de test compilés de manière traditionnelle. Cling est construit sur le dessus de LLVM et Clang, fournissant un pont pour que C++ puisse être utilisé de manière interprétée.

## Voir également
- [Cling](https://root.cern/cling/) : Un interpréteur interactif de C++, construit sur le sommet des bibliothèques LLVM et Clang.
- [Jupyter Notebooks](https://jupyter.org/) : Offre une coquille interactive dans un environnement de notebook, supporte C++ à travers le noyau xeus-cling.
- [LLVM](https://llvm.org/) : Une collection de technologies de compilateurs et de chaînes d'outils modulaires et réutilisables, sur lesquelles Cling repose.
