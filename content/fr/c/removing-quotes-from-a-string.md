---
title:                "Retirer les guillemets d'une chaîne"
date:                  2024-01-26T03:37:49.928583-07:00
model:                 gpt-4-0125-preview
simple_title:         "Retirer les guillemets d'une chaîne"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/c/removing-quotes-from-a-string.md"
---

{{< edit_this_page >}}

## Quoi & Pourquoi ?

Supprimer les guillemets d'une chaîne signifie retirer toutes les marques de citation—que ce soit simple ('') ou double ("")—qui font partie du contenu de la chaîne. Les programmeurs font cela pour assainir les entrées, préparer les données pour un traitement ultérieur, ou éviter les erreurs de syntaxe lorsqu'ils traitent avec des chemins de fichiers et des commandes dans des langues qui utilisent des guillemets pour délimiter les chaînes.

## Comment faire :

Voici une fonction C qui éliminera ces guillemets encombrants de vos chaînes :

```c
#include <stdio.h>
#include <string.h>

void remove_quotes(char *str) {
    char *p_read = str, *p_write = str;
    while (*p_read) {
        if (*p_read != '"' && *p_read != '\'') {
            *p_write++ = *p_read;
        }
        p_read++;
    }
    *p_write = '\0';
}

int main() {
    char str[] = "He said, \"Hello, 'world'!\"";
    printf("Origine : %s\n", str);
    remove_quotes(str);
    printf("Assaini : %s\n", str);
    return 0;
}
```

Exemple de sortie :

```
Origine : He said, "Hello, 'world'!"
Assaini : He said, Hello, world!
```

## Exploration approfondie

Supprimer les guillemets d'une chaîne est une tâche présente depuis l'aube de la programmation, où l'hygiène des données était et reste clé pour éviter les erreurs (comme les attaques par injection SQL) ou s'assurer qu'une chaîne peut être transmise en toute sécurité à des systèmes qui pourraient confondre un guillemet avec un caractère de contrôle.

Historiquement, les différents langages gèrent cette tâche différemment—certains disposent de fonctions intégrées (comme `strip` en Python), tandis que d'autres, comme le C, nécessitent une mise en œuvre manuelle en raison de son accent sur le contrôle de bas niveau offert aux développeurs.

Les alternatives incluent l'utilisation de fonctions de bibliothèque comme `strpbrk` pour trouver les guillemets ou l'emploi d'expressions régulières (avec des bibliothèques telles que PCRE) pour des motifs plus complexes, bien que cela puisse être excessif pour simplement retirer des guillemets.

L'implémentation ci-dessus scanne simplement chaque caractère dans la chaîne, copiant uniquement les caractères non-guillemets vers l'emplacement du pointeur d'écriture. C'est efficace car cela se fait sur place sans nécessiter de mémoire supplémentaire pour la chaîne de résultat.

## Voir aussi

- [Fonctions de la bibliothèque standard C](http://www.cplusplus.com/reference/clibrary/)
- [PCRE - Perl Compatible Regular Expressions](https://www.pcre.org/)
- [Comprendre les pointeurs en C](https://www.learn-c.org/en/Pointers)
- [Programmation sécurisée en C](https://owasp.org/www-project-secure-coding-in-c)