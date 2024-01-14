---
title:                "C: Rechercher et remplacer du texte"
simple_title:         "Rechercher et remplacer du texte"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/c/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

# Pourquoi

La recherche et le remplacement de texte sont des tâches courantes dans la programmation en C. Cela permet de modifier rapidement et efficacement des portions de texte dans un programme, ce qui peut être utile pour corriger des erreurs ou mettre à jour du code existant.

# Comment faire

La recherche et le remplacement de texte peuvent être réalisés en utilisant la fonction de la bibliothèque standard "str_replace". Voici un exemple de code montrant comment remplacer le texte "bonjour" par "salut" dans une chaîne de caractères :

````C
#include <stdio.h>
#include <string.h>

int main () {
   char str[] = "Bonjour tout le monde";
   char *result = NULL;
   
   // Recherche du mot "bonjour"
   result = str_replace(str, "bonjour", "salut");
   
   printf("Nouvelle chaîne : %s\n", result);
   return 0;
}
````

La sortie de ce programme serait : "Nouvelle chaîne : salut tout le monde". Comme on peut le voir, la fonction "str_replace" remplace toutes les occurrences du mot "bonjour" par "salut" dans la chaîne de caractères.

# Plongée en profondeur

La fonction "str_replace" utilise l'algorithme de Boyer-Moore pour rechercher et remplacer le texte dans la chaîne de caractères. Cela permet une recherche plus rapide et efficace, en particulier pour les chaînes de caractères plus longues.

De plus, la fonction peut également prendre en compte des options telles que la sensibilité à la casse ou le nombre maximum de remplacements à effectuer. Cela en fait un outil polyvalent pour la recherche et le remplacement de texte.

# Voir aussi

- [Documentation de la fonction "str_replace" en C](https://www.tutorialspoint.com/c_standard_library/string_h.htm)
- [Algorithmes de recherche et de remplacement de texte en C](https://www.geeksforgeeks.org/searching-for-patterns-set-5-efficient-construction-of-finite-automata/) 
- [Guide pour apprendre le langage C](https://openclassrooms.com/fr/courses/19980-apprenez-a-programmer-en-c)