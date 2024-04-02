---
date: 2024-01-20 17:52:19.451321-07:00
description: "L'affichage de sorties de d\xE9bogage, c'est exposer les entrailles\
  \ de votre code \u2013 voir ce qu'il se passe en temps r\xE9el. Les d\xE9veloppeurs\
  \ le font pour\u2026"
lastmod: '2024-03-13T22:44:58.327296-06:00'
model: gpt-4-1106-preview
summary: "L'affichage de sorties de d\xE9bogage, c'est exposer les entrailles de votre\
  \ code \u2013 voir ce qu'il se passe en temps r\xE9el. Les d\xE9veloppeurs le font\
  \ pour\u2026"
title: "Affichage des sorties de d\xE9bogage"
weight: 33
---

## What & Why?
L'affichage de sorties de débogage, c'est exposer les entrailles de votre code – voir ce qu'il se passe en temps réel. Les développeurs le font pour traquer les bugs plus efficacement.

## How to:
Pour afficher un message de débogage dans Fish, utilisez simplement `echo` ou `printf` avec votre message.

```Fish Shell
echo "Début du script"
# ... votre code...
echo "Valeur de la variable: $ma_variable"
# ... plus de votre code...
```

Sortie attendue:
```
Début du script
Valeur de la variable: truc
```

Utilisez `printf` pour plus de contrôle sur le format:

```Fish Shell
set ma_variable 42
printf "La réponse à la vie, l'univers et tout le reste est: %d\n" $ma_variable
```

Sortie attendue:
```
La réponse à la vie, l'univers et tout le reste est: 42
```

## Deep Dive
Dans l'histoire, l’affichage de sorties de débogage était souvent accompli avec des commandes simples comme `echo`. Avec le temps, les outils se sont sophistiqués, mais dans un shell, ces commandes basiques restent d'une grande aide. En Fish, l'utilisation de `echo` et `printf` est un choix naturel, mais il existe d'autres options plus avancées comme le débogage interactif avec `fish -d 3` pour augmenter le niveau de débogage.

## See Also
Pour plus d'astuces Fish ou pour approfondir vos connaissances en débogage, consultez:

- La documentation officielle de Fish: [https://fishshell.com/docs/current/index.html](https://fishshell.com/docs/current/index.html)
- Guide pour `printf` : [https://fishshell.com/docs/current/cmds/printf.html](https://fishshell.com/docs/current/cmds/printf.html)
- Info sur le débogage Fish: [https://fishshell.com/docs/current/index.html#debugging](https://fishshell.com/docs/current/index.html#debugging)
