---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 17:57:47.461359-07:00
description: "La gestion des erreurs en C consiste \xE0 d\xE9tecter et r\xE9pondre\
  \ aux conditions anomales qui surviennent pendant l'ex\xE9cution d'un programme.\
  \ Les programmeurs\u2026"
lastmod: '2024-02-25T18:49:55.004416-07:00'
model: gpt-4-0125-preview
summary: "La gestion des erreurs en C consiste \xE0 d\xE9tecter et r\xE9pondre aux\
  \ conditions anomales qui surviennent pendant l'ex\xE9cution d'un programme. Les\
  \ programmeurs\u2026"
title: Gestion des erreurs
---

{{< edit_this_page >}}

## Quoi & Pourquoi ?

La gestion des erreurs en C consiste à détecter et répondre aux conditions anomales qui surviennent pendant l'exécution d'un programme. Les programmeurs font cela pour prévenir les bugs, les plantages et les comportements imprévisibles, assurant ainsi que le logiciel fonctionne de manière fiable et efficace dans divers scénarios.

## Comment faire :

C ne dispose pas de support intégré pour les exceptions comme certains autres langages. Il repose plutôt sur quelques stratégies conventionnelles de gestion des erreurs, telles que le retour de valeurs spéciales des fonctions et la définition de variables globales comme `errno`.

**Retourner des Valeurs Spéciales**

Les fonctions peuvent indiquer des erreurs en retournant une valeur spécifique qui est peu susceptible d'être un résultat valide. Voici un exemple avec des entiers :

```c
#include <stdio.h>

int inverse(int number, double *result) {
    if (number == 0) {
        return -1; // Cas d'erreur
    } else {
        *result = 1.0 / number;
        return 0; // Succès
    }
}

int main() {
    double result;
    if (inverse(0, &result) < 0) {
        printf("Erreur : Division par zéro.\n");
    } else {
        printf("L'inverse est : %f\n", result);
    }
    
    return 0;
}
```

**Sortie :**
```
Erreur : Division par zéro.
```

**Vérifier `errno`**

Pour les fonctions de bibliothèque, surtout celles qui interagissent avec le système ou l'OS (comme les E/S de fichiers), `errno` est défini lorsqu'une erreur se produit. Pour l'utiliser, incluez `errno.h` et vérifiez `errno` après un échec suspecté :

```c
#include <stdio.h>
#include <errno.h>
#include <string.h>

int main() {
    FILE *file = fopen("nonexistent.txt", "r");
    if (file == NULL) {
        printf("Erreur lors de l'ouverture du fichier : %s\n", strerror(errno));
    }
    
    return 0;
}
```

**Sortie :**
```
Erreur lors de l'ouverture du fichier : Aucun fichier ou dossier de ce type
```

## Approfondissement

Historiquement, la conception minimaliste du langage de programmation C a exclu un mécanisme de gestion des exceptions intégré, reflétant ses origines en programmation système de bas niveau où les performances maximales et le contrôle proche du matériel sont critiques. À la place, C adopte une approche de gestion des erreurs plus manuelle qui correspond à sa philosophie d'offrir aux programmeurs autant de contrôle que possible, même au prix de la commodité.

Bien que cette approche s'aligne bien avec les objectifs de conception de C, elle peut également conduire à un code de vérification d'erreur verbeux et à la possibilité d'oublis de vérifications d'erreurs, que les langages modernes abordent avec des mécanismes de gestion des exceptions structurées. Par exemple, les exceptions dans des langages comme Java ou C# permettent un traitement centralisé des erreurs, rendant le code plus propre et la gestion des erreurs plus directe. Cependant, les exceptions introduisent leur propre surcharge et complexité, ce qui pourrait ne pas être idéal pour la programmation de niveau système où C brille.

Malgré sa rudesse, cette gestion manuelle des erreurs en C a informé la conception de la gestion des erreurs dans de nombreux autres langages, offrant un modèle où l'explicité des conditions d'erreur peut conduire à un code plus prévisible et débogable. Pour les systèmes critiques, où les échecs doivent être gérés avec grâce, le paradigme de gestion des erreurs de C—combiné aux meilleures pratiques modernes comme les bibliothèques de gestion des erreurs et les conventions—assure robustesse et fiabilité.
