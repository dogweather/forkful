---
title:    "C: Capitaliser une chaîne de caractères"
keywords: ["C"]
---

{{< edit_this_page >}}

## Pourquoi
Il peut y avoir plusieurs raisons pour lesquelles un programmeur souhaiterait capitaliser une chaîne de caractères. Cela peut être utile pour formater correctement une entrée utilisateur, rendre un texte plus lisible ou simplement pour des raisons esthétiques.

## Comment faire
Il existe plusieurs façons de capitaliser une chaîne de caractères en programmation C. Voici deux exemples:

#### 1. Utilisation de la fonction toupper()
La bibliothèque standard de C fournit une fonction appelée toupper() qui convertit un caractère en majuscule. Nous pouvons utiliser cette fonction dans une boucle pour capitaliser chaque caractère de notre chaîne.

```C
#include <stdio.h>
#include <ctype.h>

int main() {
    char str[] = "bonjour";

    printf("Avant la capitalisation : %s\n", str);
    
    for (int i = 0; str[i] != '\0'; i++) {
        str[i] = toupper(str[i]);
    }
    
    printf("Après la capitalisation : %s\n", str);
    
    return 0;
}
```

Output:
```
Avant la capitalisation : bonjour
Après la capitalisation : BONJOUR
```

#### 2. Utilisation de l'arithmétique de pointeur
Une autre méthode couramment utilisée pour capitaliser une chaîne de caractères est d'utiliser l'arithmétique de pointeur et la différence entre les valeurs ASCII des caractères en majuscule et en minuscule.

```C
#include <stdio.h>

int main() {
    char str[] = "bonjour";

    printf("Avant la capitalisation : %s\n", str);

    char *ptr = str;
    while(*ptr != '\0') {
        if (*ptr >= 'a' && *ptr <= 'z') {
            *ptr = *ptr - 32;
        }
        ptr++;
    }

    printf("Après la capitalisation : %s\n", str);

    return 0;
}
```

Output:
```
Avant la capitalisation : bonjour
Après la capitalisation : BONJOUR
```

## Plongeons plus profondément
Il est important de noter que dans ces exemples, nous avons supposé que la chaîne de caractères ne contenait que des lettres minuscules. Si la chaîne contient déjà des majuscules, la méthode utilisant toupper() ne fera que convertir les lettres minuscules en majuscules sans distinction. Cela peut être gênant dans certains cas.

En outre, ces méthodes ne fonctionnent que pour les caractères de l'alphabet anglais. Si vous travaillez avec des caractères d'autres langues, vous devrez utiliser d'autres méthodes pour capitaliser correctement la chaîne.

## Voir aussi
- [How to capitalize the first letter of a string in C](https://www.programiz.com/c-programming/examples/capitalize-first-letter-string)
- [String functions in C](https://www.tutorialspoint.com/cprogramming/c_string_functions.htm)