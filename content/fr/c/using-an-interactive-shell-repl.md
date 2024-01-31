---
title:                "Utilisation d'une console interactive (REPL)"
date:                  2024-01-26T04:11:26.111212-07:00
model:                 gpt-4-0125-preview
simple_title:         "Utilisation d'une console interactive (REPL)"

category:             "C"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/c/using-an-interactive-shell-repl.md"
---

{{< edit_this_page >}}

## Quoi et Pourquoi ?
Un shell interactif, ou boucle lire-évaluer-afficher (Read-Eval-Print Loop, REPL), est un outil fournissant un environnement de codage en temps réel pour tester instantanément des extraits de code. Les programmeurs l'utilisent pour obtenir un retour rapide pendant le développement, l'apprentissage et le débogage.

## Comment faire :
C ne dispose pas d’un REPL intégré, mais vous pouvez utiliser des outils tiers. Voici un aperçu en utilisant Cling, un interpréteur C++ qui peut également gérer le code C :

```C
#include <stdio.h>

int main() {
    printf("Bonjour, monde REPL !\n");
    return 0;
}
```

Sortie dans le REPL de Cling :
```
[cling]$ .x votrescript.c
Bonjour, monde REPL !
```

Cling exécute le script et affiche la sortie instantanément.

## Exploration approfondie
Les REPL sont standards dans les langues dynamiques comme Python ou Ruby, mais pour des langues compilées comme C, ils sont moins courants. Historiquement, le cycle compiler-exécuter-déboguer ne se prêtait pas à l'exploration interactive. Des outils comme Cling et les compilateurs C en ligne offrent des expériences similaires à un REPL en enveloppant votre code C dans un environnement C++.

Des alternatives à Cling incluent des interpréteurs C comme CINT et Ch. Ces outils permettent une itération rapide mais ne peuvent pas convenir à tous les scénarios de développement en raison de contraintes de performance et du support pour des fonctionnalités complexes.

L’implémentation d'un REPL dans un langage compilé implique de compiler et exécuter des extraits de code à la volée, ce qui n'est pas trivial et peut avoir des limitations par rapport aux capacités complètes du langage.

## Voir aussi
- Cling : https://github.com/root-project/cling
- Compilateur et REPL C en ligne : https://repl.it/languages/c
- CINT : http://root.cern.ch/drupal/content/cint
- Interpréteur Ch : http://www.softintegration.com/products/chstandard/
