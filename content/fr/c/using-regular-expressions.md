---
title:                "Utiliser des expressions régulières"
html_title:           "C: Utiliser des expressions régulières"
simple_title:         "Utiliser des expressions régulières"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/c/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Pourquoi

Les expressions régulières, aussi appelées regex, sont des outils puissants pour rechercher et manipuler du texte dans un programme C. Elles permettent de trouver des motifs spécifiques, de valider des entrées utilisateurs et de remplacer du texte dans une chaîne de caractères. Utiliser des expressions régulières peut donc rendre notre code plus performant et plus robuste.

## Comment faire

Pour utiliser des expressions régulières en C, nous avons besoin d'inclure la bibliothèque `<regex.h>` dans notre code. Cette bibliothèque nous fournit des fonctions comme `regcomp()` pour compiler une expression régulière et `regexec()` pour l'appliquer à un texte. Voici un exemple de code pour rechercher un motif de code postal dans une chaîne de caractères :

```C
#include <stdio.h>
#include <regex.h>

int main() {
  regex_t regex;
  int match;
  char *text = "Mon code postal est 12345";

  // Compile l'expression régulière
  match = regcomp(&regex, "[0-9]{5}", 0);
  if (match) {
    fprintf(stderr, "Erreur lors de la compilation de l'expression régulière\n");
    return 1;
  }

  // Applique l'expression régulière au texte
  match = regexec(&regex, text, 0, NULL, 0);
  if (!match) {
    printf("Code postal trouvé !\n");
  } else if (match == REG_NOMATCH) {
    printf("Aucun code postal trouvé\n");
  } else {
    printf("Erreur lors de l'application de l'expression régulière\n");
    return 1;
  }

  return 0;
}
```

Dans cet exemple, nous utilisons `[0-9]{5}` comme motif pour rechercher une suite de 5 chiffres dans la chaîne `text`. Si le motif est trouvé, la fonction `regexec()` renvoie 0 et nous affichons un message approprié. Sinon, si aucun motif n'est trouvé, elle renvoie `REG_NOMATCH` et si une erreur se produit, elle renvoie un code d'erreur.

## Plongée en profondeur

Il existe de nombreux caractères spéciaux et opérateurs que nous pouvons utiliser dans une expression régulière pour créer des motifs encore plus précis. Par exemple, `\d` correspond à n'importe quel chiffre, `\w` à n'importe quel caractère alphanumérique et `+` pour indiquer qu'un élément doit être présent une ou plusieurs fois. Il existe également des méthodes avancées telles que la rétro-référence, qui permettent de capturer et de réutiliser des parties d'une chaîne de caractères correspondant à un motif spécifique.

Il peut sembler un peu intimidant d'utiliser des expressions régulières pour la première fois, mais une fois que vous avez compris les bases, elles peuvent grandement simplifier la manipulation de texte dans votre code en réduisant la quantité de code nécessaire pour effectuer des tâches courantes.

## Voir aussi

- [Documentation de la bibliothèque `<regex.h>`](https://www.gnu.org/software/libc/manual/html_node/Regular-Expression-Functions.html#Regular-Expression-Functions)
- [Guide de référence rapide pour les expressions régulières en C](https://www.tutorialspoint.com/c_standard_library/c_function_regcomp.htm)
- [Outil de test en ligne pour les expressions régulières](https://regex101.com/)