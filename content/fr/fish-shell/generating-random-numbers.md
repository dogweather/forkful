---
title:    "Fish Shell: Génération de nombres aléatoires"
keywords: ["Fish Shell"]
---

{{< edit_this_page >}}

# Pourquoi

L'utilisation de nombres aléatoires est essentielle dans la programmation pour créer des jeux, des simulations et des dessins génératifs. Cela permet également de tester les fonctionnalités de nos programmes en leur fournissant des données aléatoires.

## Comment faire

Les utilisateurs de Fish Shell peuvent facilement générer des nombres aléatoires en utilisant la fonction `math/random` intégrée au shell. Voici un exemple de code et son résultat :

```Fish Shell
# Générer un nombre aléatoire entre 1 et 10
math/random 1 10
# Sortie : 6
```

Vous pouvez également utiliser cette fonction pour créer des chaînes de caractères aléatoires en spécifiant la longueur souhaitée :

```Fish Shell
# Générer une chaîne aléatoire de 10 caractères
math/random --string 10
# Sortie : Lw6*g8H3HN
```

## Plongée en profondeur

La fonction `math/random` utilise un générateur de nombres pseudo-aléatoires basé sur une formule mathématique. Ce générateur utilise un "graine" (seed) pour démarrer la génération des nombres. Si aucune graine n'est spécifiée, Fish Shell utilisera l'horloge du système comme graine par défaut.

Il est également possible de spécifier une graine manuellement en utilisant l'option `--seed` suivie d'un nombre entier.

```Fish Shell
# Générer un nombre aléatoire avec une graine spécifique
math/random --seed 1234
# Sortie : 42
```

Il est important de noter que les nombres générés par cette fonction ne sont pas complètement aléatoires et peuvent être répétés si la même graine est utilisée à chaque fois.

# Voir aussi

- [Documentation officielle de Fish Shell](https://fishshell.com/docs/current/cmds/math.html#random-generation)
- [Génération de nombres aléatoires en ligne de commande avec Fish Shell](https://www.linux.com/topic/desktop/generating-random-numbers-command-line/)
- [Introduction à la programmation générative en utilisant des nombres aléatoires](https://www.smashingmagazine.com/2015/06/generating-random-shapes-with-math-and-svg/)