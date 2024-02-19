---
aliases:
- /fr/c/using-a-debugger/
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 18:09:44.935969-07:00
description: "Les d\xE9bogueurs en C sont des outils sp\xE9cialis\xE9s qui permettent\
  \ aux d\xE9veloppeurs de parcourir leur code pas \xE0 pas, d'inspecter les variables\
  \ et de\u2026"
lastmod: 2024-02-18 23:09:09.361331
model: gpt-4-0125-preview
summary: "Les d\xE9bogueurs en C sont des outils sp\xE9cialis\xE9s qui permettent\
  \ aux d\xE9veloppeurs de parcourir leur code pas \xE0 pas, d'inspecter les variables\
  \ et de\u2026"
title: "Utiliser un d\xE9bogueur"
---

{{< edit_this_page >}}

## Quoi et pourquoi ?

Les débogueurs en C sont des outils spécialisés qui permettent aux développeurs de parcourir leur code pas à pas, d'inspecter les variables et de surveiller le flux d'exécution. Ce processus est essentiel pour identifier et corriger les bugs, en s'assurant que le code se comporte comme prévu.

## Comment faire :

GDB (GNU Debugger) est le débogueur le plus couramment utilisé pour la programmation en C. Voici un guide rapide sur l'utilisation de GDB pour déboguer un simple programme en C.

D'abord, compilez votre programme C avec le drapeau `-g` pour inclure les informations de débogage :

```c
gcc -g program.c -o program
```

Ensuite, démarrez GDB avec votre programme compilé :

```bash
gdb ./program
```

Vous pouvez maintenant utiliser diverses commandes à l'intérieur de GDB pour contrôler son fonctionnement. Voici quelques commandes fondamentales :

- `break` : Définit un point d'arrêt à une ligne ou fonction spécifiée pour interrompre l'exécution.
  - Exemple : `break 10` ou `break main`
- `run` : Commence l'exécution de votre programme dans GDB.
- `next` : Exécute la ligne de code suivante sans entrer dans les fonctions.
- `step` : Exécute la ligne de code suivante, en entrant dans les fonctions.
- `print` : Affiche la valeur d'une variable.
- `continue` : Reprend l'exécution jusqu'au prochain point d'arrêt.
- `quit` : Quitter GDB.

Voici un exemple de session de débogage d'un programme simple :

```c
#include <stdio.h>

int main() {
    int i;
    for (i = 0; i < 5; i++) {
        printf("%d\n", i);
    }
    return 0;
}
```

Compilez et démarrez GDB comme décrit. Définissez un point d'arrêt à la ligne `printf` avec `break 5` puis `run`. Utilisez `next` pour parcourir la boucle et `print i` pour inspecter la variable de la boucle.

Exemple de sortie après avoir défini un point d'arrêt et avant la première itération :

```
Breakpoint 1, main () at program.c:5
5         printf("%d\n", i);
```

En utilisant `print i` après quelques itérations :

```
$3 = 2
```

Cela démontre l'examen de l'état et du flux d'un programme simple.

## Plongée profonde

Le concept de débogage a considérablement évolué depuis les débuts de la programmation, où des bugs physiques (insectes littéraux) pouvaient causer des problèmes dans les ordinateurs mécaniques. Aujourd'hui, des débogueurs comme GDB offrent des fonctionnalités sophistiquées au-delà des étapes de base et de l'inspection des variables, telles que le débogage inverse (exécuter le programme à l'envers), les points d'arrêt conditionnels et le scriptage pour les tâches de débogage automatisées.

Alors que GDB est puissant et largement utilisé, il peut être dense et difficile pour les débutants. Des outils de débogage alternatifs et des IDE (Environnements de Développement Intégrés) tels que Visual Studio Code, CLion ou Eclipse offrent des interfaces plus conviviales pour le débogage du code C, intégrant souvent des aides visuelles et des contrôles plus intuitifs. Ces alternatives peuvent ne pas offrir toute la profondeur fonctionnelle de GDB, mais peuvent être plus accessibles aux nouveaux venus en programmation C.

De plus, l'émergence de protocoles de serveur de langage et de normes de débogage a facilité les solutions de débogage multiplateformes, rendant l'expérience de débogage plus cohérente à travers différents outils et environnements. Malgré ces avancées, apprendre les tenants et les aboutissants d'un débogueur traditionnel comme GDB offre une perspicacité inestimable sur l'exécution des programmes C et reste une compétence cruciale dans la boîte à outils d'un développeur.
