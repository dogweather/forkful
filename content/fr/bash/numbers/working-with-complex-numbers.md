---
aliases:
- /fr/bash/working-with-complex-numbers/
date: 2024-01-26 04:36:39.816396-07:00
description: "Les nombres complexes se composent d'une partie r\xE9elle et d'une partie\
  \ imaginaire. Les programmeurs les utilisent dans des domaines comme le traitement\u2026"
lastmod: 2024-02-18 23:09:09.010043
model: gpt-4-0125-preview
summary: "Les nombres complexes se composent d'une partie r\xE9elle et d'une partie\
  \ imaginaire. Les programmeurs les utilisent dans des domaines comme le traitement\u2026"
title: Manipulation des nombres complexes
---

{{< edit_this_page >}}

## Quoi & Pourquoi ?
Les nombres complexes se composent d'une partie réelle et d'une partie imaginaire. Les programmeurs les utilisent dans des domaines comme le traitement des signaux, la mécanique quantique, et chaque fois que le calcul les exige, parce que les nombres réels normaux ne suffisent tout simplement pas.

## Comment faire :
Bash ne prend pas en charge les nombres complexes de manière native. Vous utiliserez souvent un outil externe comme `bc` avec son option `-l`. Voici comment manipuler les nombres complexes dans bash :

```bash
echo "sqrt(-1)" | bc -l
```

Sortie :
```bash
j
```

Multiplication :

```bash
echo "(-1 + -1i) * (4 + 3i)" | bc -l
```

Sortie :
```bash
-1.00000000000000000000-7.00000000000000000000i
```

## Plongée Profonde
Les nombres complexes existent depuis le 16e siècle, mais les langages de script comme Bash ne sont pas prêts pour des calculs mathématiques comme les nombres complexes par défaut. C'est pourquoi `bc` ou d'autres outils comme `awk` entrent souvent en jeu. Quelques langages alternatifs pour travailler avec les nombres complexes incluent Python avec son module `cmath` et MATLAB, qui sont tous deux conçus pour des fonctions mathématiques plus avancées. Quant à Bash, il s'agit de tirer parti des outils - `bc` utilise le 'i' minuscule pour représenter l'unité imaginaire et prend en charge les opérations de base comme l'addition, la soustraction, la multiplication et la division.

## Voir Aussi
- Le manuel de `bc` : https://www.gnu.org/software/bc/manual/html_mono/bc.html
- GNU Octave (alternative à MATLAB) : https://www.gnu.org/software/octave/
- Le module `cmath` de Python : https://docs.python.org/3/library/cmath.html
