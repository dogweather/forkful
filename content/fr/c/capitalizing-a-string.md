---
title:                "Mettre une chaîne de caractères en majuscules"
html_title:           "C: Mettre une chaîne de caractères en majuscules"
simple_title:         "Mettre une chaîne de caractères en majuscules"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/c/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Qu'est-ce et Pourquoi ?
Capitaliser une chaîne se traduit par transformer toutes les lettres minuscules en majuscules. Les programmeurs le font principalement pour uniformiser les entrées de données textuelles, ainsi que pour les comparaisons de chaînes insensibles à la casse. 

## Comment faire:
Voici un exemple de code qui illustre comment mettre en majuscule une chaîne en utilisant la bibliothèque C standard `ctype.h`.

```C
#include <stdio.h>
#include <ctype.h>
#include <string.h>

void capitalizeString(char s[]) {
   for (int i = 0; s[i]!='\0'; i++) {
      s[i] = toupper(s[i]);
   }
}

int main() {
   char string[] = "bonjour, monde";
   capitalizeString(string);
   printf("%s\n", string); // Output: BONJOUR, MONDE
   return 0;
}
```

## Plongée en profondeur 
Historiquement, la capitalisation des chaînes en C a été effectuée à l'aide de la fonction `toupper()`, qui est une partie de la bibliothèque C standard `ctype.h`. Cette fonction ne convertit qu'un seul caractère à la fois, donc une boucle est nécessaire pour traverser toute la chaîne.

Quand il s'agit d'alternatives, vous pouvez également créer votre propre fonction pour capitaliser une chaîne. Cependant, l'utilisation de `toupper()` est préférée car elle est standard, bien testée et généralement plus optimisée.

Le point clé ici est que `toupper()` manipule les valeurs ASCII des caractères. Pour les lettres minuscules (a-z), les valeurs ASCII vont de 97 à 122, et pour les lettres majuscules (A-Z), de 65 à 90. Donc, techniquement, `toupper()` soustrait simplement 32 de la valeur ASCII d'une lettre minuscule pour obtenir sa contrepartie majuscule.

## Voir Aussi 
1. [Bibliothèque ctype.h](http://www.cplusplus.com/reference/cctype/)
2. [Chaînes de caractères en C](https://en.wikipedia.org/wiki/C_string_handling)
3. [ASCII - Table des caractères](https://www.ascii-code.com/)