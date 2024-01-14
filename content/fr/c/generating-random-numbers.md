---
title:    "C: Génération de nombres aléatoires"
keywords: ["C"]
---

{{< edit_this_page >}}

# Pourquoi

La génération de nombres aléatoires est une compétence fondamentale pour tout programmeur en langage C. Elle permet de créer des jeux, des simulations ou encore des tests aléatoires. Cela peut sembler anodin, mais c'est une compétence très utile dans le développement de logiciels complexes.

# Comment Faire

La première étape pour générer des nombres aléatoires en C est d'inclure la bibliothèque `stdlib.h`. Cette bibliothèque contient des fonctions utiles pour la génération de nombres aléatoires. Ensuite, il vous suffit d'utiliser la fonction `rand()` pour générer un nombre aléatoire compris entre 0 et `RAND_MAX` (une constante définie dans la bibliothèque `stdlib.h`).

Voici un exemple de code en C pour générer 10 nombres aléatoires :

```C
#include <stdio.h>
#include <stdlib.h>

int main() 
{
    int i;
    for(i = 0; i < 10; i++) {
        
        // Génère un nombre aléatoire compris entre 0 et RAND_MAX
        int random_number = rand();
        
        // Affiche le nombre aléatoire
        printf("%d\n", random_number);
    }
    return 0;
}
```

Voici un exemple de sortie possible :

```
3742
5097
48
231
30715
0
1474
20574
798
3416
```

Comme vous pouvez le constater, les nombres générés sont différents à chaque exécution du programme. La fonction `rand()` utilise en fait une "graine" pour générer des nombres aléatoires. Si vous voulez garantir une séquence de nombres aléatoires identique à chaque exécution, vous pouvez utiliser la fonction `srand()` en lui passant une graine spécifique comme argument.

# Plongée Profonde

Maintenant que vous savez comment générer des nombres aléatoires en C, voici quelques informations supplémentaires pour vous aider à mieux comprendre le fonctionnement de la fonction `rand()`.

- `RAND_MAX` est une constante définie dans la bibliothèque `stdlib.h` qui correspond à la valeur maximale pouvant être générée par la fonction `rand()`. Sa valeur peut varier selon les compilateurs, mais elle est généralement très grande (au moins 32767).
- La fonction `rand()` utilise un algorithme pseudo-aléatoire pour générer des nombres. Cela signifie que bien qu'ils semblent être aléatoires, les nombres générés suivent en fait une séquence prévisible. Cela peut être utile pour déboguer votre code en reproduisant un comportement spécifique.
- Si vous souhaitez générer des nombres aléatoires dans un intervalle spécifique, vous pouvez utiliser la formule suivante : `rand() % (max - min + 1) + min`. Par exemple, si vous voulez générer un nombre aléatoire compris entre 1 et 10, vous pouvez utiliser `rand() % 10 + 1`.

# Voir Aussi

- [Documentation de la fonction `rand()` en français](https://www.tutorialspoint.com/c_standard_library/c_function_rand.htm)
- [Tutoriel sur la génération de nombres aléatoires en C sur developpez.com](https://developpez.net/forums/d786295/autres-langages/c/generation-nombre-aleatoire-c/)
- [Tutoriel interactif sur la génération de nombres aléatoires en C sur learnc.org](https://www.learn-c.org/fr/Generer_des_nombres_aleatoires)