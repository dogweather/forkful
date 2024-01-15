---
title:                "Conversion d'une chaîne en minuscules"
html_title:           "C: Conversion d'une chaîne en minuscules"
simple_title:         "Conversion d'une chaîne en minuscules"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/c/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Why

Si vous travaillez avec des chaînes de caractères dans votre code C, vous pourriez avoir besoin de les convertir en minuscules à un moment donné. Cela peut être utile pour effectuer des comparaisons de chaînes de caractères insensibles à la casse ou pour s'assurer que toutes les chaînes sont uniformes dans leur formatage.

## Comment faire

Pour convertir une chaîne de caractères en minuscules en utilisant le langage C, vous pouvez utiliser la fonction `tolower()` de la bibliothèque standard `ctype.h`. Cette fonction prend un caractère en entrée et renvoie sa version en minuscule. Pour utiliser cette fonction sur une chaîne de caractères entière, vous pouvez utiliser une boucle pour itérer sur chaque caractère et appliquer `tolower()` à chaque itération.

Exemple de code :

```C
#include <stdio.h>
#include <ctype.h>

int main(void) {
    char str[] = "C PROGRAMMING";
    
    for (int i = 0; str[i] != '\0'; i++) {
        str[i] = tolower(str[i]);
    }
    
    printf("%s", str); // affiche "c programming"
    
    return 0;
}
```

## Plongée en profondeur

Outre la fonction `tolower()`, il existe également la fonction `toupper()` dans la bibliothèque `ctype.h` qui convertit un caractère en majuscule. Ces deux fonctions reconnaissent les spécificités de la langue du système sur lequel le code est exécuté, c'est-à-dire qu'elles prendront en compte les caractères accentués par exemple.

De plus, si vous avez besoin de convertir une chaîne de caractères en minuscules sans la modifier, vous pouvez utiliser la fonction `tolower()` mais en utilisant une variable temporaire pour stocker la version convertie. Cela est utile lorsqu'on souhaite conserver la chaîne d'origine tout en en ayant une version en minuscules pour une comparaison ultérieure par exemple.

## Voir aussi

- [Documentation de la fonction `tolower()` en français](https://www.manpage.fr/man/3/tolower/)
- [Documentation de la bibliothèque `ctype.h` en français](https://www.manpage.fr/man/3/ctype.h/)