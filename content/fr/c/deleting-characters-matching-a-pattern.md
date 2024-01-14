---
title:    "C: Suppression de caractères correspondant à un modèle"
keywords: ["C"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fr/c/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

# Pourquoi
Supprimer des caractères correspondant à un modèle peut être utile lorsque vous souhaitez nettoyer ou formater une chaîne de caractères. Cela peut également être utile pour des tâches de manipulation de données, telles que la suppression de symboles inutiles dans un ensemble de données ou la numérisation de texte pour des programmes de traitement automatique du langage naturel.

## Comment faire
Voici un exemple de code en C montrant comment supprimer les caractères correspondant au modèle « xyz » dans une chaîne de caractères :
```C
#include <stdio.h>
#include <string.h>

void delete_pattern(char *string, const char *pattern) { // La fonction prend une chaîne de caractères et le motif à supprimer en tant que paramètres
    char *match; // Pointeur pour stocker l'emplacement du motif correspondant

    while ((match = strstr(string, pattern)) != NULL) { // Rechercher le motif dans la chaîne
        memmove(match, match + strlen(pattern), strlen(match) - strlen(pattern) + 1); // Déplacer tous les caractères après le motif vers la gauche
    }
}

int main() {
    char string[] = "abcxyzdefxyzghi"; // Exemple de chaîne de caractères
    const char pattern[] = "xyz"; // Motif à supprimer
    
    delete_pattern(string, pattern); // Appel de la fonction
    
    printf("%s", string); // Afficher la chaîne de caractères après suppression du motif
    
    return 0;
}
```
La sortie de ce code serait `abcdefghi`.
Vous pouvez également modifier la fonction pour qu'elle supprime tous les caractères correspondant au motif, et non seulement la première occurrence. Cela peut être utile si vous avez plusieurs occurrences d'un motif dans une chaîne de caractères. Vous pouvez également utiliser cette technique pour supprimer des caractères spécifiques, pas seulement un motif.

## Plongée en profondeur
Pour ceux qui sont intéressés par les détails techniques, voici une explication plus détaillée du code :
- La fonction `delete_pattern` prend en paramètres une chaîne de caractères et un motif sous forme de pointeurs de caractères. Les pointeurs sont utilisés pour éviter une copie inutile de la chaîne de caractères en mémoire.
- `strstr` est une fonction en C qui recherche un motif dans une chaîne de caractères et renvoie un pointeur vers la première occurrence de ce motif. Elle renvoie `NULL` si le motif n'est pas trouvé.
- `strlen` est une fonction en C qui renvoie la longueur d'une chaîne de caractères.
- `memmove` est une fonction en C qui copie une partie d'une chaîne de caractères vers une autre partie de la même chaîne. Dans ce cas, elle est utilisée pour déplacer tous les caractères après le motif vers la gauche, en écrasant le motif lui-même.
- La boucle `while` s'exécute tant que la fonction `strstr` renvoie un pointeur (c'est-à-dire tant que le motif est trouvé dans la chaîne).
- La fonction `memmove` déplace tous les caractères après le motif, y compris le caractère nul de fin de chaîne. C'est pourquoi nous avons ajouté `+ 1` à `strlen(pattern)` pour s'assurer que le caractère nul de fin de chaîne n'est pas supprimé.
- À la fin de la boucle, la chaîne de caractères ne contient plus de motif correspondant et est prête à être utilisée.

# Voir aussi
- [Documentation de la fonction strstr en C](https://www.tutorialspoint.com/c_standard_library/c_function_strstr.htm)
- [Documentation de la fonction memmove en C](https://www.tutorialspoint.com/c_standard_library/c_function_memmove.htm)
- [Autres méthodes pour supprimer des caractères dans une chaîne en C](https://stackoverflow.com/questions/16362548/suppress-characters-in-c-string/16362764#16362764)