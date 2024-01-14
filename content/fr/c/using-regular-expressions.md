---
title:    "C: Utiliser des expressions régulières"
keywords: ["C"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fr/c/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Pourquoi

Si vous êtes un programmeur C, vous avez probablement entendu parler de l'utilisation d'expressions régulières dans votre code. Mais pourquoi devriez-vous vous intéresser à leur utilisation ? Les expressions régulières peuvent être un outil puissant pour rechercher et manipuler des chaînes de caractères dans un programme.

## Comment faire

Pour utiliser des expressions régulières en C, vous devez d'abord inclure la bibliothèque d'expressions régulières en utilisant la directive `#include <regex.h>`. Ensuite, vous pouvez utiliser les fonctions `regcomp()` et `regexec()` pour compiler et exécuter des expressions régulières respectivement.

```C
#include <stdio.h>
#include <regex.h>

int main() {
    regex_t regex;
    char *pattern = "hello";
    char *string = "hello world";
    char *result;
    
    // Compile la regex
    int success = regcomp(&regex, pattern, 0);
    if (success) {
        printf("Erreur de compilation de la regex\n");
        return 1;
    }
    
    // Recherche de la regex dans la chaîne de caractères
    regmatch_t matches[1];
    if (regexec(&regex, string, 1, matches, 0) == 0) {
        result = "Match trouvé";
    } else {
        result = "Aucun match trouvé";
    }
    
    // Affiche le résultat
    printf("%s\n", result);
    
    // Libère la mémoire allouée pour la regex
    regfree(&regex);
    
    return 0;
}
```

La sortie de ce programme sera "Match trouvé" car la chaîne de caractères contient la sous-chaîne "hello". Vous pouvez également utiliser des expressions régulières pour capturer des groupes spécifiques de caractères dans une chaîne.

## Plongée en profondeur

La clé pour maîtriser les expressions régulières en C est de comprendre la syntaxe et les différents modificateurs disponibles. Par exemple, le modificateur `i` peut être utilisé pour rechercher de manière insensible à la casse, tandis que `w` permet de rechercher seulement des mots entiers. De plus, les caractères spéciaux comme `*`, `+` et `.*` ont des significations spécifiques dans les expressions régulières.

Il est également important de comprendre les caractères d'échappement et les expressions régulières de plage, qui vous permettent de spécifier une plage de caractères à rechercher. En étudiant quelques exemples et en pratiquant, vous pourrez maîtriser l'utilisation de ces outils puissants dans votre code C.

## Voir aussi

- [Documentation de la bibliothèque d'expressions régulières en C](https://www.gnu.org/software/libc/manual/html_node/Regular-Expressions.html)
- [Guide pratique pour les expressions régulières en C](https://linux.die.net/Beta4/regex.html)
- [Tests et exercices pour pratiquer les expressions régulières en C](https://www.geeksforgeeks.org/regular-expressions-in-c-programming/)
- [Exemples de code pour les expressions régulières en C](http://www.zytra.net/regex/)

En utilisant ces ressources et en étant patient, vous pourrez devenir un expert en matière d'expressions régulières en C. Alors n'hésitez pas à les utiliser dans votre code pour rendre vos programmes encore plus puissants !