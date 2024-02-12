---
title:                "Vérifier si un répertoire existe"
aliases:
- /fr/c/checking-if-a-directory-exists.md
date:                  2024-02-03T17:52:36.273929-07:00
model:                 gpt-4-0125-preview
simple_title:         "Vérifier si un répertoire existe"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/c/checking-if-a-directory-exists.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Quoi & Pourquoi ?

Vérifier si un répertoire existe en C implique d'interroger le système de fichiers pour vérifier si un chemin spécifique mène à un répertoire. Les programmeurs effectuent souvent cette opération pour s'assurer que les opérations sur les fichiers (telles que la lecture ou l'écriture de fichiers) sont dirigées vers des chemins valides, prévenant les erreurs et améliorant la fiabilité du logiciel.

## Comment faire :

En C, l'existence d'un répertoire peut être vérifiée en utilisant la fonction `stat`, qui récupère des informations sur le fichier ou le répertoire à un chemin spécifié. La macro `S_ISDIR` de `sys/stat.h` est ensuite utilisée pour évaluer si les informations récupérées correspondent à un répertoire.

Voici comment vous pouvez utiliser `stat` et `S_ISDIR` pour vérifier si un répertoire existe :

```c
#include <stdio.h>
#include <sys/stat.h>

int main() {
    struct stat stats;
    
    // Chemin du répertoire à vérifier
    char *dirPath = "/chemin/vers/repertoire";

    // Obtenir le statut du chemin
    int result = stat(dirPath, &stats);

    // Vérifier si le répertoire existe
    if (result == 0 && S_ISDIR(stats.st_mode)) {
        printf("Le répertoire existe.\n");
    } else {
        printf("Le répertoire n'existe pas.\n");
    }

    return 0;
}
```

Sortie d'exemple :
```
Le répertoire existe.
```

Ou, si le répertoire n'existe pas :
```
Le répertoire n'existe pas.
```

## Approfondissement :

La structure et la fonction `stat` font partie du langage de programmation C depuis des décennies, dérivant d'Unix. Elles fournissent un moyen standardisé de récupérer des informations sur le système de fichiers, qui, bien qu'étant relativement bas niveau, est largement utilisé en raison de sa simplicité et de son accès direct aux métadonnées du système de fichiers.

Historiquement, vérifier l'existence et les propriétés des fichiers et des répertoires avec `stat` et ses dérivés (comme `fstat` et `lstat`) a été une approche commune. Cependant, ces fonctions interagissent directement avec le noyau de l'OS, ce qui peut introduire une surcharge et des erreurs potentielles si elles ne sont pas correctement gérées.

Pour les nouveaux projets ou lors du travail dans des scénarios de haut niveau, les programmeurs pourraient opter pour des mécanismes de gestion de fichiers plus abstraits fournis par des cadres modernes ou des bibliothèques qui gèrent les erreurs plus élégamment et fournissent une API plus simple. Néanmoins, comprendre et pouvoir utiliser `stat` reste une compétence précieuse pour des scénarios nécessitant une manipulation directe du système de fichiers, tels que la programmation de systèmes ou lorsque l'on travaille dans des environnements contraints où les dépendances sur de grandes bibliothèques sont irréalisables.
