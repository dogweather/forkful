---
title:                "Concaténation de chaînes de caractères"
html_title:           "C: Concaténation de chaînes de caractères"
simple_title:         "Concaténation de chaînes de caractères"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/c/concatenating-strings.md"
---

{{< edit_this_page >}}

## Qu'est-ce que c'est et pourquoi le faire?

La concaténation de chaînes est une technique couramment utilisée par les programmeurs pour combiner plusieurs chaînes de caractères en une seule. Cela peut être utile dans de nombreuses situations, comme la création d'un message d'erreur ou la manipulation de données.

## Comment faire:

Il existe plusieurs façons de concaténer des chaînes en C. Voici un exemple de code utilisant la fonction `strcat` qui prend deux chaînes en entrée et les concatène ensemble:

```C
char str1[10] = "Hello ";
char str2[] = "World!";
strcat(str1, str2);
printf("%s", str1);
```

Le résultat de ce code serait "Hello World!". Vous pouvez également concaténer des chaînes en utilisant l'opérateur `+` pour les pointeurs de chaîne, comme dans cet exemple:

```C
char str1[10] = "Hello ";
char str2[] = "World!";
printf("%s", str1 + str2);
```

## Plongée en profondeur:

La concaténation de chaînes existe depuis longtemps dans la programmation. Dans les premiers langages comme Fortran et COBOL, cela se faisait en utilisant des fonctions spécifiques telles que `CONCAT` et `MERGE`. Les langages modernes apportent souvent des fonctionnalités plus avancées telles que les chaînes de caractères immuables et les opérateurs de surcharge pour la concaténation.

Dans certains cas, il peut être avantageux d'utiliser des alternatives à la concaténation de chaînes, comme l'utilisation de tableaux de caractères ou de listes chaînées pour stocker des données. Cela peut être plus efficace en termes de performances et de gestion de la mémoire.

En termes d'implémentation, la fonction `strcat` utilisée dans l'exemple précédent nécessite que la chaîne de destination ait suffisamment d'espace pour accueillir la chaîne concaténée. Dans les cas où cela n'est pas garanti, il est préférable d'utiliser `strncat` qui prend également un troisième argument pour spécifier la longueur maximale de la chaîne résultante.

## Voir aussi:

- [Documentation C: Concaténation de chaînes de caractères](https://www.gnu.org/software/libc/manual/html_node/Concatenating-Strings.html)
- [Différences entre les chaînes de caractères et les tableaux de caractères en C](https://hackr.io/blog/learn-c-programming-strings)