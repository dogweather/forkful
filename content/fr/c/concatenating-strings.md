---
title:                "Concaténation de chaînes de caractères"
html_title:           "C: Concaténation de chaînes de caractères"
simple_title:         "Concaténation de chaînes de caractères"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/c/concatenating-strings.md"
---

{{< edit_this_page >}}

## Pourquoi

Concaténer des chaînes de caractères est une pratique courante en programmation, car cela permet de combiner plusieurs chaînes pour former une seule chaîne plus longue. Cela peut être utile, par exemple, pour créer des messages dynamiques ou pour assembler des fichiers.

## Comment faire

Pour concaténer des chaînes en C, nous pouvons utiliser la fonction `strcat()`, qui se trouve dans la bibliothèque standard `string.h`. Cette fonction prend deux paramètres : la chaîne de destination, qui sera modifiée pour inclure la chaîne concaténée, et la chaîne source, qui sera ajoutée à la fin de la chaîne de destination. Voici un exemple de code :

```C
#include <stdio.h>
#include <string.h>

int main() {
    char str1[20] = "Bonjour ";
    char str2[] = "à tous !";

    strcat(str1, str2); // Concaténation des deux chaînes

    printf("%s", str1); // Affiche "Bonjour à tous !"

    return 0;
}
```

Dans cet exemple, `str1` est la chaîne de destination et `str2` est la chaîne source. La fonction `strcat()` modifie `str1` pour inclure la chaîne `str2` à la fin. N'oubliez pas de déclarer les chaînes avec une taille suffisamment grande pour accueillir la chaîne finale, sinon vous risquez de causer des erreurs de mémoire.

## Plongée en profondeur

En plus de la fonction `strcat()`, il existe d'autres moyens de concaténer des chaînes de caractères en C. Par exemple, vous pouvez utiliser l'opérateur `+` pour concaténer des chaînes littérales avec des variables de type `char*`. Cependant, cette méthode n'est pas recommandée car elle peut causer des problèmes de mémoire si les chaînes ne sont pas correctement gérées.

De plus, il est important de noter que la fonction `strcat()` peut être inefficace en termes de performances si elle est utilisée de manière répétée pour concaténer de grandes chaînes. Dans ce cas, il est préférable d'utiliser la fonction `sprintf()`, qui permet de concaténer des chaînes tout en spécifiant la taille maximale de la chaîne de sortie.

## Voir aussi

- [Documentation de la fonction strcat() en C](https://www.tutorialspoint.com/c_standard_library/c_function_strcat.htm)
- [Exemples de concaténation de chaînes en C](https://www.geeksforgeeks.org/concatenate-strings-in-c-3-different-ways/)
- [Bonnes pratiques pour la manipulation des chaînes en C](https://www.gidnetwork.com/b-61.html)