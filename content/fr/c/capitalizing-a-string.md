---
title:                "C: Capitaliser une chaîne"
programming_language: "C"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/c/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Pourquoi

Lorsqu'on travaille en programmation, on peut être amené à manipuler des chaînes de caractères (ou "strings" en anglais). Pour diverses raisons, on peut avoir besoin de mettre en majuscule une partie ou la totalité d'une chaîne de caractères. Dans ce cas, il est utile de connaître la méthode pour le faire en langage C.

## Comment faire

Pour mettre en majuscule une chaîne de caractères en C, on peut utiliser la fonction `toupper()` de la bibliothèque standard `ctype.h`. Voici un exemple de code :

```C
#include <stdio.h>
#include <ctype.h>

int main(void)
{
    char str[] = "Bonjour tout le monde";
    int i;

    // parcours de la chaîne de caractères
    for (i = 0; str[i] != '\0'; i++)
    {
        printf("%c", toupper(str[i]));  // utilisation de la fonction toupper()
    }

    return 0;
}
```

Voici le résultat qui s'affichera à l'exécution de ce code :

```
BONJOUR TOUT LE MONDE
```

## Plongée en profondeur

Il est important de noter que cette méthode ne modifie pas la chaîne de caractères initiale mais en crée une nouvelle mise en majuscule. Si l'on souhaite modifier la chaîne d'origine, il faudra utiliser la fonction `toupper()` en prenant en compte la position des caractères dans la chaîne.

De plus, il est également possible d'utiliser la fonction `tolower()` pour mettre en minuscule une chaîne de caractères.

## Voir aussi

- [Documentation de la fonction `toupper()` en français](https://www.tutorialspoint.com/c_standard_library/ctype_h.htm)
- [Documentation de la fonction `tolower()` en français](https://www.tutorialspoint.com/c_standard_library/ctype_h.htm)
- [Exemples de manipulation de chaînes de caractères en C](https://www.tutorialspoint.com/cprogramming/c_strings.htm)