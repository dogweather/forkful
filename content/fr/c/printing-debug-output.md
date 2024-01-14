---
title:                "C: Afficher les résultats de débogage"
simple_title:         "Afficher les résultats de débogage"
programming_language: "C"
category:             "C"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/c/printing-debug-output.md"
---

{{< edit_this_page >}}

## Pourquoi 

Si vous êtes un programmeur, vous avez probablement déjà entendu parler de l'impression de sortie de débogage (debug output). Mais pourquoi le faire? Eh bien, c'est une technique utile pour trouver et résoudre les erreurs dans votre code. En imprimant des messages de débogage, vous pouvez suivre le flux d'exécution de votre programme et repérer où quelque chose ne se passe pas comme prévu. Cela peut vous faire gagner beaucoup de temps et de frustration lors du débogage de votre code.

## Comment faire 

La première étape pour imprimer un message de débogage est d'utiliser la fonction `printf()` de la bibliothèque standard en C. Cette fonction accepte un format et les valeurs à imprimer, puis les affiche à l'écran. Voici un exemple simple:

```C
#include <stdio.h>
int main() {
  int i = 42;
  printf("La réponse ultime est %d\n", i);
  return 0;
}
```

La sortie de ce programme serait `La réponse ultime est 42`. Cette fonction peut également être utilisée pour afficher les valeurs de variables ou de structures plus complexes. Vous pouvez également utiliser la fonction `fprintf()` pour imprimer dans un fichier plutôt que sur la sortie standard.

Il est important de noter que vous devez être prudent lorsque vous imprimez du débogage, car cela peut causer des problèmes de performances ou de fuite de mémoire s'il est utilisé à mauvais escient. Assurez-vous de retirer tous les messages de débogage avant de publier votre code en production.

## Plongée en profondeur 

Bien qu'il soit simple d'utiliser `printf()` pour imprimer des messages de débogage, il existe des techniques plus avancées pour rendre cette tâche encore plus efficace. Par exemple, vous pouvez utiliser des macros prédéfinies telles que `__FILE__` et `__LINE__` pour imprimer le nom du fichier et le numéro de ligne à partir desquels un message est imprimé, ce qui peut être utile pour localiser les erreurs dans un code complexe.

Vous pouvez également utiliser des commandes de débogage telles que `assert()` pour arrêter l'exécution de votre programme à un point spécifique si une condition n'est pas remplie. Ces techniques peuvent être particulièrement utiles lorsque vous avez besoin de déboguer un programme en cours d'exécution.

## Voir aussi

- [Guide de débogage C pour les débutants](https://www.freecodecamp.org/news/a-beginners-guide-to-debugging-c-programming/)
- [Documentation officielle de la fonction `printf()`](https://www.cplusplus.com/reference/cstdio/printf/)
- [Conseils pour utiliser des macros de débogage en C](https://embeddedartistry.com/blog/2018/07/12/c-macros-for-debug-printing/)