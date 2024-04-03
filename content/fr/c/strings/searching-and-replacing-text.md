---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 18:08:11.461639-07:00
description: "La recherche et le remplacement de texte en C consiste \xE0 identifier\
  \ des sous-cha\xEEnes sp\xE9cifiques au sein d'une cha\xEEne plus grande et \xE0\
  \ les substituer par\u2026"
lastmod: '2024-03-13T22:44:58.354373-06:00'
model: gpt-4-0125-preview
summary: "La recherche et le remplacement de texte en C consiste \xE0 identifier des\
  \ sous-cha\xEEnes sp\xE9cifiques au sein d'une cha\xEEne plus grande et \xE0 les\
  \ substituer par diff\xE9rentes sous-cha\xEEnes."
title: Rechercher et remplacer du texte
weight: 10
---

## Comment faire :
C ne dispose pas de fonctions intégrées pour effectuer directement la recherche et le remplacement sur des chaînes. Cependant, vous pouvez y parvenir en combinant diverses fonctions de manipulation de chaînes disponibles dans la bibliothèque `<string.h>` ainsi que de la logique personnalisée. Ci-dessous se trouve un exemple basique de comment rechercher une sous-chaîne dans une chaîne et la remplacer. Pour simplifier, cet exemple suppose une taille de tampon suffisante et ne gère pas les problèmes d'allocation de mémoire, que vous devriez considérer dans le code de production.

```c
#include <stdio.h>
#include <string.h>
#include <stdlib.h>

void replaceSubstring(char *source, char *sub, char *new_sub) {
    char tampon[1024];
    char *point_d_insertion = &tampon[0];
    const char *tmp = source;
    size_t len_sub = strlen(sub), len_new_sub = strlen(new_sub);
    size_t len_jusqu_au_correspondance;

    while ((tmp = strstr(tmp, sub))) {
        // Calculer la longueur jusqu'à la correspondance
        len_jusqu_au_correspondance = tmp - source;
        
        // Copier la partie avant la correspondance
        memcpy(point_d_insertion, source, len_jusqu_au_correspondance);
        point_d_insertion += len_jusqu_au_correspondance;
        
        // Copier la nouvelle sous-chaîne
        memcpy(point_d_insertion, new_sub, len_new_sub);
        point_d_insertion += len_new_sub;
        
        // Passer au-delà de la correspondance dans la chaîne source
        tmp += len_sub;
        source = tmp;
    }
    
    // Copier toute partie restante de la chaîne source
    strcpy(point_d_insertion, source);
    
    // Imprimer la chaîne modifiée
    printf("Chaîne modifiée : %s\n", tampon);
}

int main() {
    char sourceStr[] = "Bonjour, ceci est un test. Ce test est simple.";
    char sub[] = "test";
    char newSub[] = "exemple";
    
    replaceSubstring(sourceStr, sub, newSub);
    
    return 0;
}
```

Sortie exemple :
```
Chaîne modifiée : Bonjour, ceci est un exemple. Cet exemple est simple.
```

Ce code démontre une approche simple pour rechercher toutes les instances d'une sous-chaîne (`sub`) dans une chaîne source et les remplacer par une autre sous-chaîne (`newSub`), en utilisant la fonction `strstr` pour trouver le point de départ de chaque correspondance. C'est un exemple très basique qui ne gère pas des scénarios complexes tels que les sous-chaînes chevauchantes.

## Plongée Profonde
L'approche utilisée dans la section "Comment faire" est fondamentale, illustrant comment réaliser la recherche et le remplacement de texte en C sans aucune bibliothèque tierce. Historiquement, du fait de l'accent mis par C sur la gestion de la mémoire de bas niveau et la performance, sa bibliothèque standard n'encapsule pas les fonctionnalités de manipulation de chaînes de haut niveau comme celles que l'on trouve dans des langues telles que Python ou JavaScript. Les programmeurs doivent gérer manuellement la mémoire et combiner diverses opérations de chaînes pour atteindre les résultats souhaités, ce qui augmente la complexité mais offre plus de contrôle et d'efficacité.

Il est crucial de noter que cette approche manuelle peut être sujette à erreurs, particulièrement lors de la gestion des allocations de mémoire et des tailles de tampon. Une manipulation incorrecte peut conduire à des dépassements de tampon et à la corruption de la mémoire, rendant le code vulnérable à des risques de sécurité.

Dans de nombreux scénarios pratiques, en particulier ceux nécessitant un traitement de texte complexe, il vaut souvent la peine de considérer l'intégration de bibliothèques tierces comme PCRE (Perl Compatible Regular Expressions) pour une recherche et remplacement basés sur les expressions régulières, ce qui peut simplifier le code et réduire le potentiel d'erreurs. De plus, les normes et les compilateurs C modernes offrent de plus en plus de fonctions intégrées et des alternatives plus sûres pour la manipulation de chaînes, visant à atténuer les écueils communs observés dans les anciens codes C. Pourtant, la compréhension fondamentale du traitement manuel du texte reste une compétence précieuse dans la boîte à outils d'un programmeur, surtout pour l'optimisation des applications critiques en termes de performance.
