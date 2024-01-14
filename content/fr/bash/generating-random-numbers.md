---
title:    "Bash: La génération de nombres aléatoires"
keywords: ["Bash"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fr/bash/generating-random-numbers.md"
---

{{< edit_this_page >}}

# Pourquoi générer des nombres aléatoires en Bash ?

Générer des nombres aléatoires peut être utile dans de nombreuses situations de programmation en Bash. Par exemple, cela peut être utile pour choisir un élément aléatoire parmi une liste, simuler des événements aléatoires ou encore créer des jeux.

## Comment générer des nombres aléatoires en Bash

Il existe plusieurs façons de générer des nombres aléatoires en Bash. La première méthode est d'utiliser la commande `shuf` qui permet de mélanger une liste de nombres de manière aléatoire. Par exemple, pour générer un nombre aléatoire entre 1 et 10, on peut utiliser la commande suivante :

```Bash
shuf -i 1-10 -n 1
```

Cette commande va créer une liste de nombres allant de 1 à 10 grâce à l'option `-i`, puis va sélectionner un nombre aléatoire grâce à l'option `-n`. On peut ensuite stocker ce nombre dans une variable pour l'utiliser dans notre programme.

## Plongée plus profonde : les différents types de générateurs de nombres aléatoires

Il existe deux types de générateurs de nombres aléatoires en Bash : les générateurs pseudo-aléatoires et les générateurs aléatoires réels.

Les générateurs pseudo-aléatoires utilisent une formule mathématique pour calculer une séquence de nombres qui semblent être aléatoires. Cependant, ils sont déterministes, c'est-à-dire que si on leur donne la même "graine" ou "seed" (nombre de départ), ils produiront toujours la même séquence de nombres. Cela les rend utiles pour les simulations ou les jeux.

Les générateurs aléatoires réels, quant à eux, utilisent des sources externes telles que l'heure actuelle, la température ou d'autres mesures pour générer des nombres aléatoires. Ils sont donc plus imprévisibles et peuvent être utilisés pour des tâches de cryptographie ou de sécurité.

# Voir aussi

- [Documentation de la commande `shuf`](https://www.computerhope.com/unix/shuf.htm)
- [Générateur de code aléatoire en Bash](https://pastebin.com/kU3Jf8NC)
- [Génération de nombres aléatoires avec l'outil `dd`](https://www.commandlinefu.com/commands/view/1043/randomly-generate-a-128-bit-encryption-key)