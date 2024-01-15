---
title:                "Génération de nombres aléatoires"
html_title:           "C: Génération de nombres aléatoires"
simple_title:         "Génération de nombres aléatoires"
programming_language: "C"
category:             "C"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/c/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Pourquoi

Si vous êtes un programmeur en herbe (ou même expérimenté) dans le domaine de la programmation C, vous vous êtes sûrement déjà demandé pourquoi on utilise des nombres aléatoires dans nos programmes. La réponse est simple : les nombres aléatoires ajoutent de la variété et de l'imprévisibilité à nos programmes, leur permettant ainsi de réaliser différentes tâches en fonction des situations aléatoires. Dans cet article, nous allons voir comment générer des nombres aléatoires en langage C et plonger un peu plus en profondeur dans le fonctionnement de ces nombres.

## Comment Faire

Pour générer des nombres aléatoires en C, nous allons utiliser la bibliothèque "stdlib.h" qui contient des fonctions pour la gestion de la mémoire et la génération de nombres aléatoires.

Pour commencer, nous devons initialiser le générateur de nombres aléatoires à l'aide de la fonction "srand". Cette fonction prend en paramètre une graine (seed) qui va permettre de générer des séquences de nombres aléatoires différentes à chaque exécution du programme. Nous pouvons utiliser la fonction "time" de la bibliothèque "time.h" pour obtenir un nombre différent à chaque fois que nous exécutons notre programme.

```
#include <stdlib.h>
#include <time.h>

int main()
{
    int seed = time(NULL);
    srand(seed);
    //code pour la génération de nombres aléatoires
}
```

Maintenant que notre générateur est initialisé, nous pouvons utiliser la fonction "rand" pour générer des nombres aléatoires. Cette fonction retourne un entier pseudo-aléatoire compris entre 0 et la constante "RAND_MAX". Pour obtenir un nombre dans une plage précise, nous pouvons utiliser le modulo (%) pour limiter le nombre retourné à la plage souhaitée.

```
int aleatoire = rand() % 10; // retourne un nombre compris entre 0 et 9
```

Vous pouvez également utiliser la fonction "rand" en combinaison avec d'autres fonctions pour obtenir des nombres aléatoires plus complexes, tels que des nombres à virgule ou des nombres dans une plage précise.

## Plongée en Profondeur

Maintenant que nous avons vu comment générer des nombres aléatoires en C, il est important de comprendre comment cette méthode fonctionne réellement.

En utilisant la fonction "rand", nous générons des nombres pseudo-aléatoires, c'est-à-dire que ces nombres sont produits de façon déterministe en utilisant une formule mathématique complexe, à partir de la graine que nous avons fournie avec la fonction "srand". Cela signifie que ces nombres ne sont pas véritablement aléatoires, mais plutôt imprévisibles pour un observateur extérieur.

De plus, le générateur de nombres aléatoires de la bibliothèque "stdlib.h" a une période limitée, ce qui signifie qu'il y aura un moment où les nombres se répéteront. Il est donc conseillé de réinitialiser le générateur régulièrement (par exemple à chaque exécution du programme).

## Voir Aussi

- [Documentation officielle de la fonction "srand" en C](https://www.gnu.org/software/libc/manual/html_node/Pseudo_002dRandom-Numbers.html)
- [Article sur la génération de nombres aléatoires en C++](https://www.geeksforgeeks.org/rand-and-srand-in-ccpp/)
- [Vidéo explicative sur le fonctionnement des nombres aléatoires en informatique](https://www.youtube.com/watch?v=RU7z7EsVl5U)

---

*Article rédigé en français par un programmeur passionné, pour les programmeurs en herbe.*