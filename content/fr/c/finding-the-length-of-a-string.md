---
title:    "C: Trouver la longueur d'une chaîne de caractères"
keywords: ["C"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fr/c/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Pourquoi

Trouver la longueur d'une chaîne de caractères peut sembler être une tâche simple en programmation, mais cela peut en réalité être très utile dans de nombreux cas. Par exemple, cela peut être nécessaire lorsque vous devez vous assurer que votre string n'excède pas une certaine limite de caractères ou lorsque vous travaillez avec des données saisies par un utilisateur.

## Comment faire

La fonction standard `strlen()` peut être utilisée pour trouver la longueur d'une chaîne de caractères en C. Voici un exemple de code pour l'utiliser:

```C
#include <stdio.h>
#include <string.h> // nécessaire pour la fonction strlen()

int main() {
    char str[] = "Bonjour!";
    int len = strlen(str);

    printf("La longueur de la chaîne est: %d", len);

    return 0;
}
```

La sortie de ce code serait: `La longueur de la chaîne est: 8`, car il y a 8 caractères dans la chaîne "Bonjour!" (y compris l'espace).

Assurez-vous d'inclure le fichier d'en-tête `<string.h>` dans votre programme pour pouvoir utiliser la fonction `strlen()`. Vous pouvez également créer votre propre fonction pour trouver la longueur d'une chaîne de caractères en utilisant une boucle `while` et en comptant le nombre de caractères jusqu'à atteindre le caractère de fin de chaîne `'\0'`.

## Plongée en profondeur

Il est important de noter que la fonction `strlen()` ne compte que les caractères jusqu'au caractère de fin de chaîne `'\0'`. Par conséquent, si vous avez une chaîne avec des espaces ou des caractères spéciaux après le caractère de fin de chaîne, ceux-ci ne seront pas comptés dans la longueur de la chaîne.

De plus, la fonction `strlen()` ne tient pas compte de la mémoire utilisée par la chaîne. Par exemple, une chaîne de 100 caractères aura une longueur de 100 selon la fonction `strlen()`, mais elle peut en réalité occuper plus de mémoire si elle est stockée en tant que chaîne Unicode.

## Voir aussi

- [Documentation de la fonction strlen() en français](https://www.cplusplus.com/reference/cstring/strlen/)
- [Tutoriel sur les chaînes de caractères en C en français](https://openclassrooms.com/fr/courses/19980-apprenez-a-programmer-en-c/17198-les-cha-nes-de-caract-res)
- [Exemple de création d'une fonction pour trouver la longueur d'une chaîne de caractères en C](https://stackoverflow.com/questions/2525524/how-to-find-the-length-of-a-string-in-c)