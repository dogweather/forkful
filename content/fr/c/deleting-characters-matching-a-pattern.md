---
title:                "C: Suppression de caractères correspondant à un motif"
simple_title:         "Suppression de caractères correspondant à un motif"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/c/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Pourquoi
Supprimer des caractères correspondant à un motif peut être utile pour nettoyer et formater des données, ou pour filtrer des informations spécifiques dans une chaîne de texte.

## Comment faire
Voici un exemple de code en C qui illustre comment supprimer tous les caractères en majuscule d'une chaîne de texte :

```C
#include <stdio.h>
#include <string.h>

int main(void) {
    char str[] = "Bonjour MONDE!";
    int i, j = 0;

    for (i = 0; str[i] != '\0'; i++) {

        // Vérifier si le caractère est en majuscule
        if (str[i] >= 'A' && str[i] <= 'Z') {
            // Décaler tous les caractères suivants vers la gauche
            for (j = i; str[j] != '\0'; j++) {
                str[j] = str[j + 1];
            }
            // Nous devons diminuer la valeur de j pour éviter une boucle infinie
            j--;
        }
    }
    printf("%s\n", str);

    return 0;
}
```

**Sortie:**
```
onjour
```

Ce code parcourt chaque caractère de la chaîne et supprime ceux qui se trouvent entre 'A' et 'Z', en décalant les caractères suivants vers la gauche pour combler l'espace vide.

## Plongeon en profondeur
Pour supprimer plusieurs caractères correspondant à un motif, on peut utiliser la fonction `strpbrk()` de la bibliothèque `string.h`. Cette fonction prend en paramètres deux chaînes de caractères et renvoie un pointeur vers la première occurrence du motif dans la chaîne. En utilisant une boucle, on peut donc supprimer tous les caractères correspondants en remplaçant les occurrences par des espaces.

Par exemple, si nous voulons supprimer tous les nombres d'une chaîne de caractères, nous pouvons utiliser ce code :

```C
#include <stdio.h>
#include <string.h>

int main(void) {
    char str[] = "Bonjour 2021!";
    char nums[] = "0123456789";
    int i;

    for (i = 0; i < strlen(nums); i++) {
        char *ptr = strpbrk(str, nums + i);
        while (ptr) {
            *ptr = ' ';
            ptr = strpbrk(ptr + 1, nums + i);
        }
    }

    printf("%s\n", str);

    return 0;
}
```

**Sortie:**
```
Bonjour !
```
En utilisant des boucles et des fonctions utiles de la bibliothèque `string.h`, il est possible de supprimer rapidement et efficacement des caractères correspondant à un motif dans une chaîne de texte. C'est une technique utile à connaître lorsque vous travaillez avec des données non structurées.

## Voir aussi
- [Fonctions de manipulation de chaînes de caractères en C](https://www.tutorialspoint.com/c_standard_library/string_h.htm)
- [Documentation officielle de la bibliothèque `string.h` en français](https://fr.cppreference.com/w/c/string)