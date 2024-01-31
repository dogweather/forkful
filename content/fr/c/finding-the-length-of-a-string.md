---
title:                "Trouver la longueur d'une chaîne de caractères"
date:                  2024-01-20T17:46:49.047999-07:00
model:                 gpt-4-1106-preview
simple_title:         "Trouver la longueur d'une chaîne de caractères"

category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/c/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Quoi et Pourquoi ?
En C, calculer la longueur d'une chaîne c'est déterminer le nombre de caractères avant le caractère nul '\0'. Cette info est essentielle quand on manipule du texte : pour tout, de la validation d'input à la gestion de la mémoire.

## Comment faire :
```C
#include <stdio.h>
#include <string.h>

int main() {
    char mot[] = "Bonjour";
    int longueur = strlen(mot);

    printf("La longueur de '%s' est de %d.\n", mot, longueur);

    return 0;
}
```
Sortie :
```
La longueur de 'Bonjour' est de 7.
```

## Exploration
Historiquement, les strings en C sont des tableaux de caractères terminés par un caractère nul, une convention établie dans les premières versions du langage. Alternativement, on pourrait parcourir la chaîne avec une boucle jusqu'à trouver le caractère nul :

```C
int string_length(const char *str) {
    const char *ptr = str;
    while (*ptr) ++ptr;
    return ptr - str;
}
```

Ça c'est la version manuelle, sans `strlen`, utile si on cherche à éviter les librairies standard pour du code embarqué ou hyper optimisé. Mais attention, `strlen` est souvent optimisée par les compilateurs, donc difficile à battre en performance sans utiliser des astuces spécifiques au matériel.

## Voir Aussi
- Documentation officielle de la fonction `strlen`: https://en.cppreference.com/w/c/string/byte/strlen
- Une discussion sur Stack Overflow sur l'optimisation de `strlen`: https://stackoverflow.com/questions/47116974/strlen-implementation-in-c-and-why-it-is-so-fast
- Pour ceux qui aiment les détails: "The C Programming Language" par Brian W. Kernighan et Dennis M. Ritchie, où le traitement des chaînes en C est décrit par ses créateurs.
