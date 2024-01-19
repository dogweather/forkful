---
title:                "Concaténation de chaînes"
html_title:           "C: Concaténation de chaînes"
simple_title:         "Concaténation de chaînes"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/c/concatenating-strings.md"
---

{{< edit_this_page >}}

## Quoi & Pourquoi?

La concaténation de chaînes en C est l'union de deux ou plus de chaînes en une seule. Les programmeurs la font pour manipuler des données de texte, organiser du code et répondre à des besoins spécifiques du programme.

## Comment faire:

Voici comment concaténer des chaînes en C:

```C
#include <stdio.h>
#include <string.h>

int main() {
    char s1[100] = "Programmer en ", s2[] = "C'est fun!";
  
    strcat(s1, s2); // Concaténer s1 et s2

    printf("%s\n", s1); // Afficher le résultat

    return 0;
}
```

La sortie du programme sera:
"Programmer en C'est fun!"

## Plongeons un peu plus loin:

1. Contexte historique: La fonction strcat() a toujours été au cœur de l'API des chaînes en C depuis K&R C (avant ANSI/ISO C).  

2. Alternatives: On pourrait utiliser strncat() pour une concaténation sûre qui evite le dépassement de mémoire tampon.

```C
strncat(s1, s2, sizeof(s1)-strlen(s1)-1);
```

3. Détails de mise en œuvre: strcat() parcourt la première chaîne jusqu'à '\0 ', puis commence à y copier la seconde chaîne. Cela nécessite un espace suffisant à la fin de la première chaîne.

## Pour aller plus loin:

- Documentation C: [http://tigcc.ticalc.org/doc/stdlib.html#string_h](http://tigcc.ticalc.org/doc/stdlib.html#string_h)
- Discussion du dépassement de mémoire tampon: [https://fr.wikipedia.org/wiki/Dépassement_de_tampon](https://fr.wikipedia.org/wiki/Dépassement_de_tampon)
- K&R C par Brian W. Kernighan et Dennis M. Ritchie: [https://www.amazon.fr/Programmation-langage-Brian-W-Kernighan/dp/2100073513](https://www.amazon.fr/Programmation-langage-Brian-W-Kernighan/dp/2100073513)