---
date: 2024-01-20 17:38:04.891766-07:00
description: "How to: En Bash, on peut convertir une cha\xEEne en minuscules avec\
  \ des manipulations de variables ou des commandes externes."
lastmod: '2024-03-13T22:44:57.974758-06:00'
model: gpt-4-1106-preview
summary: "En Bash, on peut convertir une cha\xEEne en minuscules avec des manipulations\
  \ de variables ou des commandes externes."
title: "Conversion d'une cha\xEEne de caract\xE8res en minuscules"
weight: 4
---

## How to:
En Bash, on peut convertir une chaîne en minuscules avec des manipulations de variables ou des commandes externes.

### Avec les manipulations de variables :
```Bash
texte="Bonjour, Monde!"
echo "${texte,,}"
```
Sortie :
```
bonjour, monde!
```

### Avec `tr` :
```Bash
echo "Bonjour, Monde!" | tr '[:upper:]' '[:lower:]'
```
Sortie :
```
bonjour, monde!
```

### Avec `awk` :
```Bash
echo "Bonjour, Monde!" | awk '{print tolower($0)}'
```
Sortie :
```
bonjour, monde!
```

## Deep Dive
Historiquement, la conversion en minuscules était indispensable pour les systèmes où la casse était significative. Aujourd'hui, normaliser les chaînes facilite les recherches, le tri et les opérations de correspondance.

### Alternatives
Avant Bash 4, on utilisait souvent des commandes externes comme `tr` ou `awk`. Depuis, les manipulations de variables Bash offrent une syntaxe simplifiée.

### Détails d'implémentation
Bash utilise les tables de caractères Unicode, assurant le bon fonctionnement des conversions de chaînes dans de nombreuses langues, ce qui n'est pas toujours le cas avec des commandes externes.

## See Also
- La référence du shell Bash : https://www.gnu.org/software/bash/manual/bash.html#Shell-Parameter-Expansion
- Documentation `tr` : https://www.gnu.org/software/coreutils/manual/html_node/tr-invocation.html
- Documentation `awk`: https://www.gnu.org/software/gawk/manual/gawk.html
