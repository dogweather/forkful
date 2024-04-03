---
date: 2024-01-26 04:12:07.822909-07:00
description: "Comment faire : C++ ne vient pas avec un REPL int\xE9gr\xE9, mais des\
  \ outils comme Cling offrent cette capacit\xE9. Voici comment utiliser Cling pour\
  \ calculer la\u2026"
lastmod: '2024-03-13T22:44:58.163539-06:00'
model: gpt-4-0125-preview
summary: "C++ ne vient pas avec un REPL int\xE9gr\xE9, mais des outils comme Cling\
  \ offrent cette capacit\xE9."
title: Utilisation d'une console interactive (REPL)
weight: 34
---

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
