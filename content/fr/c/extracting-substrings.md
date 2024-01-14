---
title:                "C: Extraction de sous-chaines"
programming_language: "C"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/c/extracting-substrings.md"
---

{{< edit_this_page >}}

## Pourquoi

Extrayez des sous-chaînes de caractères pour améliorer l'efficacité et la précision de votre programme en utilisant une méthode simple et utile en C.

## Comment le faire

```C
// Définition des variables pour la chaîne de caractères et la sous-chaîne
char chaine[] = "Exemple de sous-chaîne";
char sousChaine[7];

// Utilisation de la fonction strncpy pour extraire la sous-chaîne
strncpy(sousChaine, chaine + 8, 6); // copiera les 6 caractères de la chaîne à partir de la 8ème position dans la sous-chaîne

// Affichage de la sous-chaîne extraite
printf("La sous-chaîne extraite est : %s\n", sousChaine);
```

Résultat :

```
La sous-chaîne extraite est : sous-c
```

## Plongée en profondeur

La fonction `strncpy` est utilisée pour copier une certaine quantité de caractères d'une chaîne de caractères source vers une chaîne de caractères destination. Cette fonction prend trois paramètres : la chaîne destination, la chaîne source et le nombre de caractères à copier. Elle est particulièrement utile pour extraire des sous-chaînes de chaînes de caractères plus grandes.

Il est important de noter que la sous-chaîne extraite sera toujours suivie d'un caractère de fin de chaîne `'\0'`, qui sera ajouté automatiquement par la fonction `strncpy`. Il est donc important de définir la taille de la sous-chaîne avec une marge suffisante pour inclure ce caractère.

## Voir aussi

- [Documentation de la fonction `strncpy` en français](https://www.tutorialspoint.com/c_standard_library/c_function_strncpy.htm)
- [Guide de référence pour les fonctions de chaînes de caractères en C](https://www.tutorialspoint.com/c_standard_library/string_h.htm)