---
title:    "Fish Shell: Génération de nombres aléatoires"
keywords: ["Fish Shell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fr/fish-shell/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Pourquoi 

Générer des nombres aléatoires est un élément essentiel de la programmation pour de nombreuses raisons. Que vous créiez des jeux, des simulations ou des tests, avoir une source fiable de nombres aléatoires est primordial. La coquille Fish propose une méthode simple et efficace pour générer des nombres aléatoires dans vos scripts.

## Comment faire 

Pour générer un nombre aléatoire dans la coquille Fish, nous allons utiliser la commande `math` intégrée. Cette commande calcule les équations mathématiques et génère une sortie numérique. Voici un exemple de code pour générer un nombre aléatoire entre 1 et 10 :

```Fish Shell
math /dev/random '* 10' | awk '{print $1 % 10}'
```

La partie `math /dev/random '* 10'` génère un nombre aléatoire entre 0 et 9 à l'aide du générateur de nombres aléatoires de votre système d'exploitation. Nous utilisons ensuite la commande `awk` pour sélectionner uniquement le premier nombre dans la sortie. Enfin, nous utilisons le modulo pour nous assurer que le nombre final est compris entre 0 et 9. Vous pouvez modifier les valeurs pour générer un nombre aléatoire dans une plage différente.

## Plongée en profondeur 

La commande `math` offre de nombreuses possibilités pour générer des nombres aléatoires. Par exemple, vous pouvez utiliser le générateur de nombres aléatoires `RAND` pour créer une liste de nombres aléatoires. Vous pouvez également utiliser la commande `seq` intégrée pour générer une liste de nombres séquentiels et les mélanger à l'aide du générateur de nombres aléatoires. La coquille Fish propose également des variables intégrées telles que `$RANDOM` pour générer des nombres aléatoires de manière encore plus simple.

## Voir aussi 

- [Documentation officielle de la coquille Fish](https://fishshell.com/docs/current/cmds/math.html)
- [Guide des commandes de la coquille Fish](https://fishshell.com/docs/current/commands.html)
- [Guide du générateur de nombres aléatoires en ligne de commande](https://www.computerhope.com/unix/random.htm)