---
title:                "Utilisation des tableaux associatifs"
date:                  2024-01-30T19:10:04.506211-07:00
model:                 gpt-4-0125-preview
simple_title:         "Utilisation des tableaux associatifs"

category:             "C"
tag:                  "Data Structures"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/c/using-associative-arrays.md"
changelog:
  - 2024-01-30, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Quoi & Pourquoi ?

Les tableaux associatifs, ou tables de hachage, sont des paires clé-valeur qui vous permettent de stocker et de récupérer des données avec une clé. Ils sont incroyablement utiles en C parce qu'ils permettent un accès aux données plus rapide comparé aux listes, surtout lorsque vous traitez une grande quantité de données.

## Comment faire :

C n’a pas de support intégré pour les tableaux associatifs comme certains autres langages, mais nous pouvons utiliser des structures et certaines fonctions de bibliothèque pour obtenir une fonctionnalité similaire. Voici une implémentation simple en utilisant la bibliothèque `uthash`, que vous devrez inclure dans votre projet.

Tout d'abord, définissez une structure pour contenir vos paires clé-valeur :

```C
#include <stdio.h>
#include "uthash.h"

typedef struct {
    int id; // Ceci sera notre clé
    char name[10]; // Ceci est la valeur associée à notre clé
    UT_hash_handle hh; // Rend cette structure hachable
} personne;
```

Ensuite, ajoutons quelques entrées et récupérons-les :

```C
int main() {
    personne *mes_personnes = NULL, *s;

    // Ajout d'une entrée
    s = (personne*)malloc(sizeof(personne));
    s->id = 1;
    strcpy(s->name, "Alice");
    HASH_ADD_INT(mes_personnes, id, s);

    // Récupération d'une entrée
    int user_id = 1;
    HASH_FIND_INT(mes_personnes, &user_id, s);
    if (s) {
        printf("Trouvé : %s\n", s->name);
    }
    
    return 0;
}
```

Le résultat d'exécution serait :

```
Trouvé : Alice
```

N'oubliez pas de libérer la mémoire allouée et de désallouer la table de hachage une fois terminé pour éviter les fuites de mémoire.

## Plongée profonde

Bien que les tableaux associatifs ne soient pas natifs en C, des bibliothèques comme `uthash` comblent assez bien cette lacune, offrant une manière assez simple d'utiliser cette fonctionnalité. Historiquement, les développeurs C devaient implémenter leur version de ces structures de données, ce qui a mené à des implémentations variées et souvent complexes, surtout pour ceux qui commencent avec le langage.

Rappelez-vous, l'efficacité de l'utilisation des tableaux associatifs en C dépend grandement de la manière dont la fonction de hachage distribue les valeurs à travers la table pour minimiser les collisions. Bien que des bibliothèques comme `uthash` offrent un bon équilibre entre la facilité d'utilisation et les performances, dans des applications critiques où la performance est primordiale, vous pourriez vouloir personnaliser ou implémenter votre propre table de hachage.

Pour des applications nécessitant une efficacité maximale, des structures de données alternatives ou même d’autres langages de programmation avec un support intégré pour les tableaux associatifs pourraient être un meilleur choix. Cependant, pour de nombreuses situations, surtout lorsque vous travaillez déjà dans un environnement C, l'utilisation d'une bibliothèque comme `uthash` offre un équilibre pratique entre performance et commodité.
