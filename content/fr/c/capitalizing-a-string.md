---
title:                "Mise en majuscule d'une chaîne de caractères."
html_title:           "C: Mise en majuscule d'une chaîne de caractères."
simple_title:         "Mise en majuscule d'une chaîne de caractères."
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/c/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Qu'est-ce que c'est et pourquoi le faire?

Capitaliser une chaîne de caractères consiste à mettre en majuscule la première lettre de chaque mot dans une phrase donnée. Les programmeurs le font pour rendre leurs chaînes de caractères plus claires et plus lisibles.

## Comment faire:

```C
#include <stdio.h>
#include <string.h>

int main() {
  char phrase[100] = "Bonjour tout le monde !";
  char *pointeur = phrase;

  while (*pointeur != '\0') {
    if (*pointeur == ' ') {
      pointeur++;
      *pointeur = toupper(*pointeur);
    }
    pointeur++;
  }

  printf("La phrase capitalisée est : %s", phrase);

  return 0;
}
```

Output:
```Bonjour Tout Le Monde !```

## Plongée en profondeur:

Capitaliser les chaînes de caractères est une pratique qui remonte aux premières versions de langages de programmation tels que le BASIC et le COBOL. Il existe des alternatives telles que l'utilisation de tous les caractères en majuscule ou en minuscule, mais la capitalisation reste le moyen le plus courant d'améliorer la lisibilité des chaînes de caractères dans le code. L'implémentation peut varier d'un langage à l'autre, mais la logique générale reste la même : parcourir la chaîne de caractères, trouver les espaces et mettre en majuscule le caractère suivant.

## À voir également:

[La documentation de la fonction toupper en C](https://www.tutorialspoint.com/c_standard_library/c_function_toupper.htm)

[Norme de style de code en C](https://fr.wikipedia.org/wiki/Norme_de_codage#C)