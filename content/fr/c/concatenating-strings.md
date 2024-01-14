---
title:                "C: Concaténation de chaînes de caractères"
programming_language: "C"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/c/concatenating-strings.md"
---

{{< edit_this_page >}}

# Pourquoi

La concaténation de chaînes de caractères est une opération courante en programmation qui permet de combiner plusieurs chaînes pour en former une seule. Cette technique est particulièrement utile lors de la manipulation de données ou lors de l'affichage de messages à l'utilisateur.

# Comment faire

La concaténation de chaînes en C se fait en utilisant la fonction `strcat()` ou en utilisant l'opérateur de concaténation `+`. Voici un exemple de code montrant l'utilisation de ces deux méthodes :

````C
#include <stdio.h>
#include <string.h>

int main() {
   // Utilisation de strcat()
   char message1[25] = "Bonjour ";
   char message2[25] = "tout le monde !";
   strcat(message1, message2);
   printf("%s\n", message1);

   // Utilisation de l'opérateur +
   char message3[25] = "Bonjour ";
   char message4[25] = "à tous !";
   printf("%s\n", message3 + message4);

   return 0;
}
````

La sortie de ce programme sera :

```
Bonjour tout le monde !
Bonjour à tous !
```

# Approfondissement

Il est important de noter que lors de la concaténation de chaînes, la chaîne de destination doit être suffisamment grande pour contenir les deux chaînes concaténées. Dans le cas contraire, des problèmes de mémoire pourraient survenir. Il est également possible de concaténer plus de deux chaînes en utilisant plusieurs appels de la fonction `strcat()` ou plusieurs opérateurs `+` en cascade.

De plus, il est important de savoir que la concaténation de chaînes peut être une tâche coûteuse en termes de performance si la taille des chaînes est grande. Dans ce cas, il est préférable d'utiliser une fonction spécifique pour la concaténation de chaînes telle que `strncat()` qui permet de spécifier une longueur maximale pour la chaîne de destination.

# Voir aussi

- [Documentation sur la fonction strcat() en C](https://www.tutorialspoint.com/c_standard_library/c_function_strcat.htm)
- [Article sur la concaténation de chaînes en C++](https://www.programiz.com/cpp-programming/string-concatenation)
- [Documentation sur la fonction strncat() en C](https://www.tutorialspoint.com/c_standard_library/c_function_strncat.htm)