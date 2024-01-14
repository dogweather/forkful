---
title:                "C: Capitaliser une chaîne de caractères"
simple_title:         "Capitaliser une chaîne de caractères"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/c/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Pourquoi

La capitalisation d'une chaîne de caractères peut sembler une tâche simple, mais elle est en réalité très utile dans beaucoup de cas. Cela peut rendre le texte plus lisible, faciliter la recherche de certains mots et améliorer l'esthétique de votre code.

## Comment faire

Pour capitaliser une chaîne de caractères en langage C, il existe plusieurs méthodes. Voici une méthode simple en utilisant la fonction `toupper()` :

```C
#include <stdio.h>
#include <ctype.h>

int main()
{
    char string[100];
    printf("Entrez une chaîne de caractères : ");
    fgets(string, 100, stdin);

    // Boucle pour parcourir chaque caractère de la chaîne
    for (int i = 0; string[i] != '\0'; i++)
    {
        // Utilise la fonction toupper() pour convertir chaque caractère en majuscule
        string[i] = toupper(string[i]);
    }

    printf("La chaîne en majuscules est : %s", string);

    return 0;
}
```

**Output :**

```
Entrez une chaîne de caractères : Bonjour le monde
La chaîne en majuscules est : BONJOUR LE MONDE
```

Il est également possible d'utiliser la bibliothèque `string.h` pour utiliser la fonction `strlwr()` et convertir la chaîne en minuscules, puis utiliser `toupper()` pour la capitaliser.

## Plongée en profondeur

La fonction `toupper()` fait partie des fonctions de la bibliothèque `ctype.h` en langage C. Elle est utilisée pour convertir un caractère en majuscule. Cette fonction est très utile lors de la manipulation de chaînes de caractères, en particulier lorsqu'on veut comparer des chaînes sans se soucier de la casse.

Il est important de noter que la fonction `toupper()` ne modifie pas la chaîne d'origine, mais renvoie plutôt une nouvelle valeur. Ainsi, pour remplacer la chaîne d'origine, il faut utiliser une boucle et attribuer la valeur convertie à chaque caractère de la chaîne.

## Voir aussi

- [Tutoriel sur les chaînes de caractères en C](https://www.programiz.com/c-programming/c-strings)
- [Documentation de la fonction `toupper()`](https://www.tutorialspoint.com/c_standard_library/c_function_toupper.htm)
- [Exemples d'utilisation de la fonction `toupper()`](https://www.geeksforgeeks.org/toupper-in-c-cpp/)