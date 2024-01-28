---
title:                "Gestion des erreurs"
date:                  2024-01-26T00:36:50.713024-07:00
model:                 gpt-4-1106-preview
simple_title:         "Gestion des erreurs"
programming_language: "C"
category:             "C"
tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/c/handling-errors.md"
---

{{< edit_this_page >}}

## Quoi et pourquoi ?
La gestion des erreurs en C consiste à anticiper l'inattendu. Elle empêche les programmes de dérailler lorsqu'ils rencontrent des problèmes. Les programmeurs le font pour gérer les erreurs avec élégance et maintenir leur code fiable.

## Comment faire :

Voyons comment faire cela en C :

```C
#include <stdio.h>
#include <stdlib.h>
#include <errno.h>

int main() {
    FILE *fp = fopen("fichiernonexistant.txt", "r");
    if (fp == NULL) {
        perror("Erreur lors de l'ouverture du fichier");
        return EXIT_FAILURE;
    }
    // Faire quelque chose avec le fichier
    fclose(fp);
    return EXIT_SUCCESS;
}
```

Exemple de sortie lorsque le fichier n'existe pas :
```
Erreur lors de l'ouverture du fichier : Aucun fichier ou dossier de ce type
```

## Exploration approfondie

Aux premiers jours du C, la gestion des erreurs était rudimentaire - principalement des codes de retour et des vérifications manuelles. Puis `errno`, une variable globale mise à jour lorsque des fonctions échouent, est arrivée. Elle n'est pas sécurisée pour les threads en elle-même, c'est pourquoi les fonctions plus récentes `strerror` et `perror` ont été introduites pour un meilleur rapport d'erreurs.

Des alternatives ? Le C moderne n'est pas limité à `errno`. Il y a setjmp et longjmp pour des sauts non locaux lorsqu'un désastre se produit. Certains préfèrent définir leurs propres codes d'erreur, tandis que d'autres optent pour des structures similaires aux exceptions en C++.

Les détails d'implémentation peuvent être complexes. Par exemple, `errno` est sécurisé pour les threads dans des systèmes conformes à POSIX grâce à la magie du Stockage Local de Thread (TLS). Dans les systèmes embarqués, où les ressources sont précieuses, un code de gestion d'erreurs personnalisé peut être préféré aux approches standard qui pourraient alourdir le logiciel.

## Voir aussi

- Une plongée détaillée dans `errno` : https://en.cppreference.com/w/c/error/errno
- Pour la sécurité des threads, voir les threads POSIX et errno : http://man7.org/linux/man-pages/man3/pthread_self.3.html
- Une introduction à setjmp et longjmp : https://www.cplusplus.com/reference/csetjmp/
- Pour la gestion des exceptions en C++, consultez : https://isocpp.org/wiki/faq/exceptions
