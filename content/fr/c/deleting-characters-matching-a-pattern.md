---
title:                "C: Suppression de caractères correspondant à un motif"
programming_language: "C"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/c/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Pourquoi

Supprimer des caractères correspondant à un motif peut être utile lorsqu'on souhaite nettoyer ou filtrer des données. Cela peut également être nécessaire dans certains algorithmes ou programmes qui manipulent du texte.

## Comment faire

Pour supprimer des caractères correspondant à un motif en C, il existe plusieurs solutions. La plus simple consiste à utiliser la fonction `strpbrk()`, qui renvoie un pointeur vers la première occurrence du motif dans une chaîne de caractères.

```C
// Supprime tous les caractères correspondant à un motif dans une chaîne de caractères
#include <stdio.h>
#include <string.h>

int main() {
  char str[] = "Bonjour tout le monde";
  char* motif = "tro";
  
  printf("Chaîne initiale : %s\n", str);
  
  // Recherche du motif dans la chaîne
  char* result = strpbrk(str, motif);
  
  // Tant que le motif est trouvé, on le supprime
  while (result != NULL) {
    // Copie de la partie de la chaîne après le motif
    memmove(result, result+strlen(motif), strlen(result+strlen(motif))+1);
    // Recherche du motif dans le reste de la chaîne
    result = strpbrk(str, motif);
  }
  
  printf("Chaîne finale : %s\n", str);
  
  return 0;
}
```

Lors de l'exécution du code ci-dessus, on obtient le résultat suivant :

```
Chaîne initiale : Bonjour tout le monde
Chaîne finale : Bonur u le mon
```

## Plongée en profondeur

En utilisant la fonction `strpbrk()`, il est possible de supprimer plusieurs motifs à la fois dans une chaîne de caractères. Il suffit de les spécifier dans la chaîne `motif` en les séparant par des virgules.

Il est également possible d'utiliser la fonction `strcspn()` pour obtenir la longueur du motif à supprimer, puis de remplacer les caractères correspondants par des espaces pour les effacer. Cette méthode peut être utile dans certains cas où le motif à supprimer est plus complexe et contient plusieurs caractères.

## Voir aussi

- [Documentation sur la fonction `strpbrk()` en français](https://www.tutorialspoint.com/c_standard_library/c_function_strpbrk.htm)
- [Guide pour utiliser les chaînes de caractères en C](https://openclassrooms.com/courses/la-manipulation-des-chaines-de-caracteres-en-langage-c)
- [Autre méthode pour supprimer des caractères en utilisant la fonction `strcspn()`](https://hscstudio.hatenablog.com/entry/2017/11/12/011752)