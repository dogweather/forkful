---
title:                "Affichage des sorties de débogage"
date:                  2024-01-20T17:52:18.125885-07:00
model:                 gpt-4-1106-preview
simple_title:         "Affichage des sorties de débogage"

category:             "C"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/c/printing-debug-output.md"
---

{{< edit_this_page >}}

## What & Why? (Quoi et Pourquoi ?)
L’affichage de débogage, c'est imprimer des informations de diagnostic pendant l'exécution d'un programme. Les développeurs font ça pour suivre le flux d’exécution et repérer les bugs rapidement.

## How to: (Comment faire :)
Utilisant `printf` de la bibliothèque standard, voyons des exemples :

```C
#include <stdio.h>

int main() {
    int iteration = 0;
    for (int i = 0; i < 5; i++) {
        iteration++;
        printf("Iteration %d\n", iteration);
    }
    return 0;
}
```

Sortie attendue :

```
Iteration 1
Iteration 2
Iteration 3
Iteration 4
Iteration 5
```

Un autre exemple avec des conditions :

```C
#include <stdio.h>

int main() {
    for (int i = 0; i < 10; i++) {
        if (i % 2 == 0) {
            printf("Le nombre %d est pair.\n", i);
        } else {
            printf("Le nombre %d est impair.\n", i);
        }
    }
    return 0;
}
```

Sortie :

```
Le nombre 0 est pair.
Le nombre 1 est impair.
...
Le nombre 9 est impair.
```

## Deep Dive (Plongée Profonde)
Historiquement, `printf` vient de C originel. En débogage, c’est pratique mais pas sans défaut : c’est verbeux et peut ralentir le programme. Il existe des alternatives, comme `fprintf` pour écrire dans des fichiers ou `sprintf` pour formatter sans afficher. Niveau implémentation ? Gardez à l'esprit que l'ordre d'affichage n'est pas toujours garanti en cas de multithreading sans synchronisation appropriée.

## See Also (Voir Aussi)
- [La documentation de `printf`](https://en.cppreference.com/w/c/io/fprintf)
- GNU Debugger (GDB): [https://www.gnu.org/software/gdb/](https://www.gnu.org/software/gdb/)
- Logging Libraries, e.g., `syslog` on Unix: [https://man7.org/linux/man-pages/man3/syslog.3.html](https://man7.org/linux/man-pages/man3/syslog.3.html)

Cet article court devrait vous donner les basiques pour déboguer avec des affichages de sorties. Bon codage et bonne chasse aux bugs !
