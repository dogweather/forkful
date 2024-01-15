---
title:                "Suppression de caractères correspondant à un motif"
html_title:           "C: Suppression de caractères correspondant à un motif"
simple_title:         "Suppression de caractères correspondant à un motif"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/c/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Pourquoi

Supprimer des caractères correspondant à un motif peut être utile dans de nombreux cas. Par exemple, pour nettoyer des données en enlevant des espaces inutiles ou pour modifier un document en supprimant des lignes qui ne répondent pas à certaines exigences.

## Comment faire

Il existe plusieurs façons de supprimer des caractères correspondant à un motif en C, voici deux exemples :

```
char string[] = "Bonjour, comment ça va ?";

// Exemple 1 : Suppression des virgules
char *pattern = ",";
char *result = remove_matching_chars(string, pattern); // Renvoie "Bonjour comment ça va ?"

// Exemple 2 : Suppression des voyelles
char *pattern = "[aeiouAEIOU]";
char *result = remove_matching_chars(string, pattern); // Renvoie "Bnjr, cmment ç v ?"
```

## Un peu plus en détail

Il existe plusieurs fonctions et bibliothèques en C qui peuvent vous aider à supprimer des caractères correspondant à un motif. Par exemple, la fonction `strchr()` de la bibliothèque standard C peut être utilisée pour rechercher un caractère spécifique dans une chaîne de caractères et `strtok()` peut être utilisée pour diviser une chaîne en sous-chaînes en utilisant un délimiteur.

Il est également possible d'utiliser des expressions régulières pour définir un motif plus complexe à supprimer. La bibliothèque `regex.h` permet de définir et d'utiliser des expressions régulières en C.

## Voir aussi

- [Documentation officielle de la fonction strchr()](https://www.man7.org/linux/man-pages/man3/strchr.3.html)
- [Documentation officielle de la fonction strtok()](https://www.man7.org/linux/man-pages/man3/strtok.3.html)
- [Documentation officielle de la bibliothèque regex.h](https://www.gnu.org/software/libc/manual/html_node/Regular-Expressions.html)