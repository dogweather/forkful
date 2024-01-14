---
title:                "C: Production de nombres aléatoires"
programming_language: "C"
category:             "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/c/generating-random-numbers.md"
---

{{< edit_this_page >}}

# Pourquoi

La génération de nombres aléatoires est un élément fondamental de la programmation, que ce soit pour les jeux, les simulations ou les tests. Elle permet d'introduire une certaine imprévisibilité dans le code, ce qui peut être utile pour tester sa robustesse ou simuler des scénarios réalistes.

# Comment faire

En langage C, la génération de nombres aléatoires se fait à l'aide de la fonction `rand()` de la bibliothèque standard `stdlib.h`. Pour l'utiliser, il suffit de l'appeler et de stocker le résultat dans une variable. Voici un exemple de code générant 10 nombres aléatoires entre 0 et 99 :

```C
#include <stdio.h>
#include <stdlib.h>

int main() {
  int i;
  
  // initialisation de la graine aléatoire à partir de l'horloge du système
  srand(time(NULL));
  
  // boucle de 10 itérations
  for (i = 0; i < 10; i++) {
      int nombre = rand() % 100; // on utilise le modulo pour limiter le nombre à 99
      printf("%d ", nombre);
  }
  
  return 0;
}
```

Lorsqu'on exécute ce code, on obtient une série de nombres aléatoires à chaque exécution :

```
29 74 57 41 12 66 73 80 68 36 
```

# Plongée en profondeur

Les nombres générés par la fonction `rand()` ne sont pas vraiment aléatoires, ils sont en réalité calculés à partir d'une graine (seed) qui peut être déterminée à l'avance si aucun changement n'est apporté. Cela peut créer des patterns prévisibles si le code est exécuté plusieurs fois.

Pour résoudre ce problème, il est recommandé de modifier la graine à chaque exécution en utilisant une valeur imprévisible, comme l'horloge du système comme dans l'exemple précédent. On peut également utiliser la fonction `srand()` pour définir manuellement une graine aléatoire.

De plus, la fonction `rand()` n'est pas considérée comme étant vraiment aléatoire car elle suit une séquence prévisible. Pour obtenir de vrais nombres aléatoires, il est nécessaire d'utiliser des générateurs de nombres pseudo-aléatoires plus complexes, comme la fonction `random()` de la bibliothèque `unistd.h`.

# Voir aussi

- [Documentation officielle de la fonction `rand()`](https://linux.die.net/man/3/rand)
- [Exemples de génération de nombres aléatoires en langage C](https://www.geeksforgeeks.org/rand-and-srand-in-ccpp/)
- [Différences entre nombres pseudo-aléatoires et vrais nombres aléatoires](https://www.geeksforgeeks.org/difference-between-pseudo-random-and-true-random-number/)