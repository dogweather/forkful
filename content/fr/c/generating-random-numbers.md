---
title:                "Génération de nombres aléatoires"
date:                  2024-01-20T17:48:43.918897-07:00
model:                 gpt-4-1106-preview
simple_title:         "Génération de nombres aléatoires"
programming_language: "C"
category:             "C"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/c/generating-random-numbers.md"
---

{{< edit_this_page >}}

## What & Why? (Quoi et pourquoi ?)
Générer des nombres aléatoires, c'est simuler le hasard en informatique. Les programmeurs utilisent ces nombres pour tout, des jeux aux simulations, en passant par la sécurité informatique.

## How to: (Comment faire :)
Pour générer un nombre aléatoire en C, il faut inclure les headers `stdlib.h` et `time.h`, et utiliser les fonctions `rand()` et `srand()`.

```C
#include <stdio.h>
#include <stdlib.h>
#include <time.h>

int main() {
    srand(time(NULL)); // Initialisation du générateur.
    int nombre_aleatoire = rand() % 100; // Un nombre aléatoire entre 0 et 99.
    printf("Nombre aléatoire : %d\n", nombre_aleatoire);
    return 0;
}
```

Sample output:
```
Nombre aléatoire : 42
```

## Deep Dive (Plongée en profondeur)
Historiquement, le C utilise la fonction `rand()` pour générer des nombres aléatoires. Mais `rand()` n'est pas vraiment aléatoire ; c'est pseudo-aléatoire, basé sur une séquence prévisible si on connaît le "seed". Pour une séquence différente à chaque exécution, on utilise `srand()` avec le temps actuel comme seed. Des alternatives, comme `/dev/random` et `/dev/urandom` sous Unix, peuvent fournir de meilleures propriétés aléatoires pour la cryptographie. Enfin, C11 a introduit `<stdatomic.h>` pour la génération sûre en contexte multi-thread.

## See Also (Voir aussi)
- Documentation GNU sur `rand` et `srand`: https://www.gnu.org/software/libc/manual/html_node/ISO-Random.html
- Guide sur la génération de nombres aléatoires sécurisés: https://www.2uo.de/myths-about-urandom/
- La librairie C11 Standard: http://www.open-std.org/jtc1/sc22/wg14/www/docs/n1570.pdf
