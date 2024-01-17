---
title:                "Interpoler une chaîne de caractères"
html_title:           "C: Interpoler une chaîne de caractères"
simple_title:         "Interpoler une chaîne de caractères"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/c/interpolating-a-string.md"
---

{{< edit_this_page >}}

## What et Pourquoi?

Interpoler une chaîne de caractères, ou plus simplement l'incorporation de variables ou de données dans une chaîne, est une technique couramment utilisée par les programmeurs en C. Cela permet de rendre les chaînes de caractères plus dynamiques et personnalisées, améliorant ainsi la lisibilité et l'utilité du code.

## Comment Faire:

La syntaxe de base pour interpoler une chaîne de caractères en C est la suivante :
```c
printf("Voici une chaîne de caractères formatée: %s", variable);
```
Dans cet exemple, le symbole %s est remplacé par la valeur de la variable fournie. Voici un exemple plus complet :
```c
#include <stdio.h>
int main()
{
   int age = 27;
   char *nom = "Jean";
   printf("%s a %d ans.", nom, age);
   return 0;
}
```
Ce code affichera: "Jean a 27 ans." Vous pouvez également utiliser la fonction sprintf() pour stocker une chaîne interpolée dans une variable.

## Approfondissement:

L'interpolation de chaînes de caractères est une technique très répandue dans la programmation moderne, mais elle n'était pas disponible dans les premières versions du langage C. Auparavant, les programmeurs devaient utiliser des fonctions telles que strcat() pour combiner des chaînes de caractères. Bien qu'il existe d'autres méthodes pour formater et manipuler des chaînes, l'interpolation est souvent considérée comme la plus concise et la plus efficace.

## À Voir:

Pour en savoir plus sur l'interpolation de chaînes en C, voici quelques ressources utiles:
- [Documentation de la fonction printf](https://www.tutorialspoint.com/c_standard_library/c_function_printf.htm)
- [Exemples d'interpolation de chaînes](https://www.geeksforgeeks.org/difference-strcat-printf/)
- [Un aperçu de la manipulation de chaînes en C](https://www.tutorialspoint.com/cprogramming/c_strings.htm)