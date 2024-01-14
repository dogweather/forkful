---
title:                "Bash: Extraction de sous-chaînes"
simple_title:         "Extraction de sous-chaînes"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/bash/extracting-substrings.md"
---

{{< edit_this_page >}}

## Pourquoi

Le processus d'extraction de sous-chaînes, également appelé "substring", est une tâche courante lors de la programmation en Bash. Il s'agit essentiellement de prendre une partie d'une chaîne de caractères plus longue et de la stocker en tant que nouvelle variable, ce qui peut être très utile pour manipuler et analyser des données.

## Comment faire

Pour extraire une sous-chaîne en Bash, vous pouvez utiliser la syntaxe suivante :

```Bash
var="ma chaîne de caractères"
echo ${var:3:5}
```
Ce qui va afficher "châin" en sortie, car cela commence à la position 3 de la chaîne et prend 5 caractères.

Vous pouvez également utiliser des variables pour spécifier la position de départ et la longueur de la sous-chaîne, comme ceci :

```Bash
start=3
length=5
echo ${var:start:length}
```

## Plongée en profondeur

Il existe plusieurs variations de la syntaxe d'extraction de sous-chaîne en Bash, qui offrent différents niveaux de flexibilité dans la sélection de la partie de la chaîne souhaitée. Par exemple, vous pouvez utiliser des nombres négatifs pour compter à partir de la fin de la chaîne ou utiliser des expressions régulières pour extraire des motifs spécifiques.

De plus, l'extraction de sous-chaîne peut également être combinée avec d'autres opérations Bash telles que les boucles et les conditions pour automatiser davantage le processus et le rendre encore plus puissant.

## Voir aussi

- [La documentation officielle de Bash](https://www.gnu.org/software/bash/manual/bash.html#Shell-Parameter-Expansion)
- [Un guide détaillé sur l'extraction de sous-chaîne en Bash](https://www.tldp.org/LDP/abs/html/string-manipulation.html#SUBSTRREF)
- [Un tutoriel vidéo sur l'utilisation des expressions régulières pour extraire des sous-chaînes](https://www.youtube.com/watch?v=SvDQlk BIjI)

Merci d'avoir lu ! À la prochaine fois !