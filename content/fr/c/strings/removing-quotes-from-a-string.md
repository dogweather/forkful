---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 18:07:15.329838-07:00
description: "Comment faire : Pour enlever les guillemets d'une cha\xEEne en C, nous\
  \ parcourons la cha\xEEne, en copiant les caract\xE8res qui ne sont pas des guillemets\
  \ dans\u2026"
lastmod: '2024-03-13T22:44:58.357979-06:00'
model: gpt-4-0125-preview
summary: "Pour enlever les guillemets d'une cha\xEEne en C, nous parcourons la cha\xEE\
  ne, en copiant les caract\xE8res qui ne sont pas des guillemets dans une nouvelle\
  \ cha\xEEne."
title: "Supprimer les guillemets d'une cha\xEEne de caract\xE8res"
weight: 9
---

## Comment faire :
Pour enlever les guillemets d'une chaîne en C, nous parcourons la chaîne, en copiant les caractères qui ne sont pas des guillemets dans une nouvelle chaîne. Ce processus peut être adapté pour retirer soit juste les guillemets initiaux et finaux soit tous les guillemets présents dans la chaîne. Ci-dessous, un exemple illustratif qui démontre les deux approches :

```c
#include <stdio.h>
#include <string.h>

// Fonction pour enlever tous les guillemets d'une chaîne
void removeAllQuotes(char *source, char *dest) {
    while (*source) {
        if (*source != '"' && *source != '\'') {
            *dest++ = *source;
        }
        source++;
    }
    *dest = '\0'; // Terminer la chaîne de destination par un caractère nul
}

// Fonction pour enlever juste les guillemets initiaux et finaux d'une chaîne
void removeEdgeQuotes(char *source, char *dest) {
    size_t len = strlen(source);
    if (source[0] == '"' || source[0] == '\'') source++, len--;
    if (source[len-1] == '"' || source[len-1] == '\'') len--;
    strncpy(dest, source, len);
    dest[len] = '\0'; // Terminer la chaîne de destination par un caractère nul
}

int main() {
    char str1[] = "'Bonjour, le monde !'";
    char str2[] = "\"Programmation en C\"";
    char noQuotes1[50];
    char noQuotes2[50];
    
    removeAllQuotes(str1, noQuotes1);
    printf("Tous les guillemets enlevés : %s\n", noQuotes1);
    
    removeEdgeQuotes(str2, noQuotes2);
    printf("Guillemets initiaux et finaux enlevés : %s\n", noQuotes2);
    
    return 0;
}
```
Sortie d'exemple :
```
Tous les guillemets enlevés : Bonjour, le monde !
Guillemets initiaux et finaux enlevés : Programmation en C
```

Ces exemples montrent comment gérer à la fois le retrait de tous les guillemets présents dans la chaîne et le retrait ciblé des guillemets initiaux et finaux.

## Exploration approfondie
Le concept d'enlever les guillemets des chaînes n'a pas une profondeur historique significative en C, au-delà de ses liens avec les besoins de traitement de texte précoces. L'approche directe démontrée ici est polyvalente mais manque d'efficacité pour les très longues chaînes ou les besoins de haute performance, où une modification sur place ou des algorithmes plus avancés pourraient être préférés.

Les alternatives, comme l'utilisation de `strpbrk` pour trouver les guillemets et déplacer la partie de la chaîne qui n'en contient pas, peuvent être plus efficaces mais nécessitent une compréhension plus approfondie des pointeurs et de la gestion de la mémoire en C. De plus, l'émergence de bibliothèques d'expressions régulières a fourni un ensemble d'outils puissants pour la manipulation des chaînes, y compris l'enlèvement des guillemets. Cependant, ces bibliothèques, bien qu'efficaces, ajoutent de la complexité et une surcharge qui pourrait ne pas être nécessaire pour des tâches plus simples. Par conséquent, l'approche directe, comme illustrée, reste une compétence précieuse pour les programmeurs en C, alliant simplicité et efficacité pour de nombreux cas d'utilisation courants.
