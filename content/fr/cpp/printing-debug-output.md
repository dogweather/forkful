---
title:                "Imprimer la sortie de débogage"
html_title:           "C++: Imprimer la sortie de débogage"
simple_title:         "Imprimer la sortie de débogage"
programming_language: "C++"
category:             "C++"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/cpp/printing-debug-output.md"
---

{{< edit_this_page >}}

## Pourquoi

Vous vous êtes probablement déjà retrouvé dans une situation où votre programme ne fonctionne pas comme prévu et vous ne savez pas pourquoi. C'est là qu'imprimer des messages de débogage peut être très utile. En ajoutant des sorties de débogage à votre code, vous pouvez obtenir des informations précieuses sur l'état de vos variables et de votre programme.

## Comment faire

```C++
#include <iostream>

using namespace std;

int main() {
    int number = 5;
    
    // Afficher la valeur de la variable "number"
    cout << "Valeur de number : " << number << endl;
    
    // Afficher un message pour déboguer
    cout << "Le programme a atteint ce point." << endl;
    
    return 0;
}
```

```
/* Output :
Valeur de number : 5
Le programme a atteint ce point.
*/
```

Pour imprimer des messages de débogage dans votre code, vous pouvez utiliser la fonction `cout` de la bibliothèque standard de C++. Il suffit d'ajouter une instruction `cout` avec le message que vous souhaitez afficher, et vous pouvez également y inclure des variables pour afficher leur valeur.

## Plongée en profondeur

Il peut être tentant d'imprimer des messages de débogage pour chaque ligne de code, mais cela peut rapidement rendre votre code illisible et alourdir votre programme. Il est important de limiter l'utilisation de ces sorties à des moments clés, souvent autour de boucles ou de conditions, afin de mieux comprendre comment votre programme se déroule.

En C++, il existe également une fonction de débogage appelée `assert()`, qui vous permet de vérifier si une condition est remplie et de stopper l'exécution du programme si ce n'est pas le cas. Cela peut être utile lors du développement, mais il est recommandé de supprimer les assertions avant la mise en production de votre code.

## Voir aussi

Pour en savoir plus sur l'impression de messages de débogage en C++, vous pouvez consulter ces ressources supplémentaires :

- [Documentation sur `cout` et `endl`](https://www.cplusplus.com/reference/iostream/cout/)
- [Documentation sur `assert()`](https://www.cplusplus.com/reference/cassert/assert/)