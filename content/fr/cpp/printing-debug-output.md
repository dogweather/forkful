---
title:                "C++: Affichage des sorties de débogage."
simple_title:         "Affichage des sorties de débogage."
programming_language: "C++"
category:             "C++"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/cpp/printing-debug-output.md"
---

{{< edit_this_page >}}

## Pourquoi

Le débogage est une étape cruciale dans le processus de développement de logiciels. Il permet de détecter et de résoudre les erreurs dans le code, garantissant ainsi un fonctionnement optimal de l'application. L'impression de sorties de débogage est un moyen essentiel pour les développeurs de mieux comprendre le comportement de leur code et de trouver rapidement les problèmes.

## Comment faire

Pour imprimer des sorties de débogage en C++, vous pouvez utiliser la fonction `std::cout`. Elle permet d'afficher le contenu d'une variable ou une chaîne de caractères sur la console. Voici un exemple:

```C++
#include <iostream>
using namespace std;

int main() {
  int x = 5;
  cout << "La valeur de x est: " << x << endl;
  return 0;
}

// Output: La valeur de x est: 5
```

Vous pouvez également utiliser la fonction `printf` de la bibliothèque standard C pour imprimer des sorties de débogage. Cette méthode est plus avancée et offre plus de flexibilité en termes de formatage. Voici un exemple:

```C++
#include <stdio.h>

int main() {
  float y = 10.5;
  printf("La valeur de y est: %f\n", y);
  return 0;
}

// Output: La valeur de y est: 10.5
```

Il est également possible d'utiliser les macros de débogage telles que `assert` et `debug` pour afficher des messages d'erreur ou de débogage lors de l'exécution du code. Ces macros doivent être activées en définissant la macro `NDEBUG` à l'aide de la commande de préprocesseur `#define`.

## Plongée en profondeur

L'impression de sorties de débogage peut également être utile pour suivre l'exécution du code et repérer les problèmes de performance. Les développeurs peuvent insérer des messages de débogage à des endroits clés du code pour mesurer le temps nécessaire à l'exécution de certaines parties du programme. Cela permet de détecter les goulots d'étranglement et d'optimiser le code pour améliorer les performances globales de l'application.

Il est important de noter que l'utilisation excessive d'impressions de débogage peut ralentir l'exécution du code et entraîner des inefficacités. C'est pourquoi il est recommandé de les utiliser avec parcimonie et de les désactiver dans le code final avant de le déployer en production.

## Voir aussi

- [Guide de débogage en C++](https://www.tutorialspoint.com/cplusplus/cpp_debugging.htm)
- [Les directives de préprocesseur en C++](https://www.cplusplus.com/doc/tutorial/preprocessor/)
- [Optimisation du code en C++](https://www.geeksforgeeks.org/c-optimization-tips/)