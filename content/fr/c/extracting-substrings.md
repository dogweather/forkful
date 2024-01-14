---
title:                "C: Extraction de sous-chaînes"
simple_title:         "Extraction de sous-chaînes"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/c/extracting-substrings.md"
---

{{< edit_this_page >}}

## Pourquoi

Extrayer des sous-chaînes (ou des sous-strings) est une technique importante en programmation en C. Cela permet de manipuler des données de manière plus précise et efficace, en ne ciblant que les parties spécifiques d'une chaîne de caractères.

## Comment faire

Pour extraire une sous-chaîne en utilisant le langage de programmation C, il faut utiliser la fonction `memcpy()`. Cette fonction permet de copier une certaine quantité de caractères d'une chaîne source vers une destination. Voici un exemple de code avec une chaîne de caractères initiale "Bonjour le monde" et une sous-chaîne à extraire "le monde":

```
#include <stdio.h>
#include <string.h>

int main(void) {
  char str[] = "Bonjour le monde";
  char sub[] = "le monde";
  char dest[10];

  memcpy(dest, &str[8], 8);
  dest[8] = '\0';

  printf("%s\n", dest);

  return 0;
}
```

La sortie de ce code sera "le monde", qui est la sous-chaîne extraite.

## Plongée en profondeur

Pour comprendre parfaitement comment fonctionne la fonction `memcpy()`, il est important de comprendre les paramètres qu'elle prend en compte. Le premier paramètre est la destination, qui est la chaîne cible où les caractères seront copiés. Le deuxième paramètre est la source, qui est la chaîne d'origine à partir de laquelle les données seront copiées. Finalement, le dernier paramètre est la taille, qui spécifie le nombre de caractères à copier. Il est également important de noter que la numérotation des caractères dans une chaîne commence à partir de 0, donc pour extraire une sous-chaîne à partir du caractère 8, le paramètre de taille doit être 8.

En utilisant `memcpy()`, il est possible d'extraire des sous-chaînes de n'importe quelle longueur, en spécifiant la taille appropriée en fonction de la position du premier caractère et du dernier caractère de la sous-chaîne.

## À voir également

- [La documentation officielle de la fonction "memcpy" en C](https://www.barrgroup.com/Embedded-Systems/How-To/C-Memcpy-Function)
- [Un tutoriel détaillé sur la manipulation de chaînes de caractères en C](https://www.programiz.com/c-programming/c-strings)
- [Un autre exemple de fonction pour extraire des sous-chaînes en C](https://www.geeksforgeeks.org/strstr-in-ccpp/)