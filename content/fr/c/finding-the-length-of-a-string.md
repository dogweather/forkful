---
title:                "Trouver la longueur d'une chaîne"
html_title:           "Go: Trouver la longueur d'une chaîne"
simple_title:         "Trouver la longueur d'une chaîne"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/c/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Qu'est-ce que c'est & Pourquoi?

La recherche de la longueur d'une chaîne est l'acte de déterminer le nombre de caractères qu'elle contient. Les programmeurs le font pour manipuler efficacement les données textuelles, par exemple pour itérer sur chaque caractère ou réserver un espace mémoire adéquat.

## Comment faire:

Commençons par un exemple simple. En C, pour trouver la longueur d'une chaîne, nous utilisons la fonction `strlen()` de la bibliothèque `string.h`. Notons que `strlen()` renvoie un nombre de type `size_t`, ce qui est essentiellement un type entier non signé.

```C
#include <stdio.h>
#include <string.h>

int main() {
    char str[] = "Bonjour le monde!";
    printf("La longueur de la chaîne est: %zu\n", strlen(str));
    return 0;
}
```

Lorsque vous exécutez ce code, vous obtiendrez:

```
La longueur de la chaîne est: 17
```

## Exploration approfondie:

Historiquement, `strlen()` existe depuis les premiers jours du langage C. C'est une abréviation de "string length", qui signifie "longueur de la chaîne" en anglais.

Il y a d'autres façons de trouver la longueur d'une chaîne en C, par exemple en écrivant votre propre boucle `while` ou `for` pour compter les caractères jusqu'à ce que vous atteigniez le caractère de fin de chaîne (`'\0'`). Cependant, `strlen()` est généralement plus rapide et plus sûr car elle est implémentée de manière optimisée dans la bibliothèque standard du C.

D'un point de vue de l'implémentation, `strlen()` compte simplement le nombre de caractères dans la chaîne jusqu'à ce qu'elle atteigne le caractère de fin de chaîne (`'\0'`). En C, toutes les chaînes sont implicitement terminées par ce caractère, ce qui signifie que nous savons toujours où la chaîne se termine.

## Pour aller plus loin:

Vous pouvez consulter ces ressources pour en savoir plus sur le traitement des chaînes en C :

- [Pointeurs et Tableau de caractères](https://e-campus.vermeg.com/cours/C/cours/C/pointeurs.html)
- [Fonctions de la bibliothèque string.h](https://www.commentcamarche.net/contents/695-fonctions-de-la-bibliotheque-string-h)
- [La fonction strlen](https://www.cplusplus.com/reference/cstring/strlen/) (en anglais)