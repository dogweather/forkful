---
title:                "Trouver la longueur d'une chaîne de caractères"
html_title:           "C: Trouver la longueur d'une chaîne de caractères"
simple_title:         "Trouver la longueur d'une chaîne de caractères"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/c/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Pourquoi
Vous êtes peut-être en train d'écrire un programme qui nécessite des manipulations sur des données textuelles. Dans ce cas, savoir la longueur d'une chaîne de caractères peut être utile pour effectuer des opérations telles que la comparaison ou la recherche de sous-chaînes.

## Comment faire
Pour trouver la longueur d'une chaîne de caractères en C, il existe plusieurs façons de procéder. Voici deux exemples de code qui illustrent différentes méthodes :

```C
// Exemple 1 : utiliser la fonction strlen() de la bibliothèque string.h
#include <stdio.h>
#include <string.h>

int main() {
  char str[] = "Bonjour le monde !";
  int longueur = strlen(str);

  printf("Longueur de la chaîne : %d", longueur);
  return 0;
}
/* Output :
 Longueur de la chaîne : 19
 */

// Exemple 2 : parcourir la chaîne de caractères jusqu'à trouver le caractère nul
#include <stdio.h>

int main() {
  char str[] = "Bonjour le monde !";
  int i = 0;

  // boucle while pour parcourir la chaîne jusqu'à trouver le caractère nul
  while (str[i] != '\0') {
    i++;
  }

  printf("Longueur de la chaîne : %d", i);
  return 0;
}
/* Output :
 Longueur de la chaîne : 19
 */
```

## Plongée en profondeur
Il est important de comprendre que la fonction strlen() ne compte pas le caractère nul à la fin de la chaîne. Cela signifie que la longueur retournée n'est pas juste le nombre de caractères visibles dans la chaîne, mais plutôt le nombre total de caractères présents, y compris le caractère nul. 

Il est également important de noter que les chaînes de caractères en C sont en fait des tableaux de caractères, et leur longueur peut donc être trouvée en utilisant la boucle while dans l'exemple 2, en parcourant chaque élément du tableau jusqu'à ce que le caractère nul soit rencontré. 

## Voir aussi
- [Les chaînes de caractères en C](https://fr.wikipedia.org/wiki/Cha%C3%AEne_de_caract%C3%A8res_en_C)
- [Documentation de la fonction strlen()](https://www.tutorialspoint.com/c_standard_library/c_function_strlen.htm)