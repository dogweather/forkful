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

# Qu'est-ce que c'est et pourquoi le faire?

Générer des nombres aléatoires est une technique utilisée par les programmeurs pour créer des valeurs aléatoires dans leurs programmes. Cela peut être utile pour simuler des scénarios aléatoires, tels que les jeux de hasard ou les jeux vidéo, ou pour générer des données de test aléatoires. Les programmeurs utilisent également des nombres aléatoires pour ajouter un élément de surprise et de variation à leurs programmes.

# Comment faire:

Voici un exemple simple de génération de nombres aléatoires en utilisant la fonction rand() en C:

```c
#include <stdio.h>
#include <stdlib.h>
#include <time.h>

int main() {
  int i, n;
  time_t t;

  // Initialise le générateur de nombres aléatoires en utilisant l'heure actuelle comme graine
  srand((unsigned) time(&t));

  // Génère 10 nombres aléatoires entre 0 et 99
  printf("Nombres aléatoires : \n");
  for( i = 0 ; i < 10 ; i++ ) {
    n = rand() % (100);
    printf("%d\n", n);
  }
  
  return 0;
}
```

Voici un exemple de sortie possible:

```
Nombres aléatoires :
78
12
43
91
6
87
34
50
99
4
```

# Plongée en profondeur:

La génération de nombres aléatoires a une longue histoire et a été utilisée dans de nombreux domaines, tels que la cryptographie, la simulation et les jeux. Dans le passé, les programmeurs utilisaient souvent des méthodes basées sur des formules mathématiques pour générer des nombres aléatoires. Cependant, ces méthodes sont devenues obsolètes et peu fiables avec l'avènement de l'informatique moderne. Aujourd'hui, les langages de programmation tels que C ont des fonctions intégrées pour générer des nombres aléatoires de manière plus efficace et plus aléatoire.

Il existe également d'autres méthodes pour générer des nombres aléatoires, telles que l'utilisation de données environnementales telles que le bruit radio ou les vibrations du disque dur. Ces méthodes sont considérées comme plus aléatoires mais peuvent être plus compliquées à implémenter.

En C, la fonction rand() utilise un générateur de congruence linéaire pour produire des nombres pseudo-aléatoires. Cela signifie que les nombres générés ne sont pas vraiment aléatoires, mais suffisamment aléatoires pour la plupart des utilisations pratiques. Les nombres générés par rand() peuvent être prédits si l'état du générateur est connu. C'est pourquoi il est important d'initialiser le générateur avec une graine différente à chaque exécution du programme.

# Voir aussi:

Pour plus d'informations sur la génération de nombres aléatoires en C, vous pouvez consulter la documentation officielle de rand() sur le site de documentation du langage C. Vous pouvez également trouver des tutoriels et des exemples utiles en ligne, ainsi que des forums où vous pouvez poser des questions et discuter avec d'autres programmeurs.