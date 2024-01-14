---
title:                "C: Convertir une chaîne en minuscule"
programming_language: "C"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/c/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Pourquoi

Il y a plusieurs raisons pour lesquelles vous pourriez avoir besoin de convertir une chaîne de caractères en minuscule en programmation en C. Cela peut être utile pour comparer des chaînes de caractères sans se soucier de la casse, ou pour assurer une entrée utilisateur uniforme avant de la traiter dans votre programme.

## Comment faire

Heureusement, la bibliothèque standard du langage C fournit une fonction simple pour convertir une chaîne de caractères en minuscule : `tolower()`. Voici un exemple de code qui utilise cette fonction pour convertir une chaîne de caractères en minuscule et l'afficher :

```C
#include <stdio.h>
#include <ctype.h>

int main() {
  char str[] = "Bonjour, je suis une CHAÎNE de CARACTÈRES";

  for (int i = 0; str[i]; i++) {
    putchar(tolower(str[i]));
  }

  return 0;
}
```

La sortie de ce code serait `bonjour, je suis une chaîne de caractères`.

## Plongée plus profonde

Si vous voulez en savoir plus sur le processus de conversion des caractères en minuscule, voici quelques points à garder à l'esprit :

- La fonction `tolower()` utilise la table ASCII pour déterminer la valeur de chaque caractère. Si un caractère n'a pas de casse (comme un chiffre ou un symbole), il reste inchangé.
- Il existe également une fonction `toupper()` pour convertir une chaîne de caractères en majuscules.
- Si vous traitez des caractères non-ASCII, il est possible que `tolower()` ne fonctionne pas correctement. Dans ce cas, vous devriez envisager d'utiliser une bibliothèque de manipulation de chaînes de caractères spécifique à votre langage.
- Enfin, gardez à l'esprit que `tolower()` modifie la chaîne de caractères d'origine, vous devriez donc créer une copie si vous avez besoin de conserver la chaîne de caractères d'origine.

## Voir aussi

- [Documentation de la fonction `tolower()` en français](https://www.tutorialspoint.com/c_standard_library/c_function_tolower.htm)
- [Article sur la manipulation de chaînes de caractères en C](https://www.geeksforgeeks.org/string-manipulations-in-c-with-examples/)
- [Exemples de code pour la conversion en minuscule dans différents langages](https://rosettacode.org/wiki/Change_character_case#C)