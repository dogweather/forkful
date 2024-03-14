---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 17:53:42.557057-07:00
description: "La concat\xE9nation de cha\xEEnes en C consiste \xE0 joindre deux cha\xEE\
  nes ou plus bout \xE0 bout pour former une nouvelle cha\xEEne. Les programmeurs\
  \ effectuent cette\u2026"
lastmod: '2024-03-13T22:44:58.362565-06:00'
model: gpt-4-0125-preview
summary: "La concat\xE9nation de cha\xEEnes en C consiste \xE0 joindre deux cha\xEE\
  nes ou plus bout \xE0 bout pour former une nouvelle cha\xEEne. Les programmeurs\
  \ effectuent cette\u2026"
title: "Concat\xE9nation de cha\xEEnes"
---

{{< edit_this_page >}}

## Quoi & Pourquoi ?

La concaténation de chaînes en C consiste à joindre deux chaînes ou plus bout à bout pour former une nouvelle chaîne. Les programmeurs effectuent cette opération pour construire dynamiquement des chaînes à l'exécution, ce qui est essentiel pour la création de messages significatifs, de chemins de fichiers, ou de toute donnée assemblée à partir de diverses sources de chaînes.

## Comment procéder :

En C, les chaînes sont des tableaux de caractères se terminant par un caractère nul (`\0`). Contrairement aux langues de plus haut niveau, C ne fournit pas de fonction de concaténation de chaînes intégrée. Au lieu de cela, vous utilisez les fonctions `strcat()` ou `strncat()` de la bibliothèque `<string.h>`.

Voici un exemple simple utilisant `strcat()` :

```c
#include <stdio.h>
#include <string.h>

int main() {
    char destination[50] = "Bonjour, ";
    char source[] = "le monde !";

    strcat(destination, source);

    printf("%s\n", destination);  // Sortie : Bonjour, le monde !
    return 0;
}
```

La fonction `strcat()` prend deux arguments : la chaîne de destination (qui doit avoir assez d'espace pour contenir le résultat de la concaténation) et la chaîne source. Elle ajoute ensuite la chaîne source à la chaîne de destination.

Pour plus de contrôle sur le nombre de caractères concaténés, `strncat()` est plus sûr à utiliser :

```c
#include <stdio.h>
#include <string.h>

int main() {
    char destination[50] = "Bonjour, ";
    char source[] = "le monde !";
    int num = 3; // Nombre de caractères à ajouter

    strncat(destination, source, num);

    printf("%s\n", destination);  // Sortie : Bonjour, le
    return 0;
}
```

Cela limite la concaténation aux premiers caractères `num` de la chaîne source, aidant à prévenir les débordements de tampon.

## Examen approfondi

Les fonctions `strcat()` et `strncat()` font partie de la bibliothèque standard C depuis sa création, reflétant la nature de bas niveau du langage qui nécessite une gestion manuelle des chaînes et de la mémoire. Contrairement à de nombreux langages de programmation modernes qui traitent les chaînes de caractères comme des objets de première classe avec des opérateurs de concaténation intégrés (tels que `+` ou `.concat()`), l'approche de C nécessite une compréhension plus approfondie des pointeurs, de l'allocation mémoire et des pièges potentiels comme les débordements de tampon.

Bien que `strcat()` et `strncat()` soient largement utilisés, ils sont souvent critiqués pour leur potentiel à créer des vulnérabilités de sécurité s'ils ne sont pas utilisés avec précaution. Les débordements de tampon, où les données dépassent la mémoire allouée, peuvent conduire à des plantages ou être exploités pour l'exécution de code arbitraire. En conséquence, les programmeurs se tournent de plus en plus vers des alternatives plus sûres, telles que `snprintf()`, qui offre un comportement plus prévisible en limitant le nombre de caractères écrits dans la chaîne de destination en fonction de sa taille :

```c
char destination[50] = "Bonjour, ";
char source[] = "le monde !";
snprintf(destination + strlen(destination), sizeof(destination) - strlen(destination), "%s", source);
```

Cette méthode est plus verbeuse mais nettement plus sûre, soulignant un changement dans les pratiques de programmation en C vers la priorisation de la sécurité et de la robustesse sur la brièveté.

Malgré ces défis, la concaténation de chaînes en C est une compétence fondamentale, cruciale pour une programmation efficace dans le langage. Comprendre ses nuances et les risques associés est la clé pour maîtriser la programmation en C.
