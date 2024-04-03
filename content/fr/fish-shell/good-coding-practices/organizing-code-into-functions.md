---
changelog:
- 2024-01-28, dogweather, reviewed and added links
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 23:01:33.191826-07:00
description: "Comment faire : Dans Fish, vous \xE9crivez une fonction avec le mot-cl\xE9\
  \ `function`, vous lui donnez un nom, et vous terminez par `end`. Voici un exemple\u2026"
lastmod: '2024-03-13T22:44:58.330710-06:00'
model: gpt-4-0125-preview
summary: "Dans Fish, vous \xE9crivez une fonction avec le mot-cl\xE9 `function`, vous\
  \ lui donnez un nom, et vous terminez par `end`."
title: Organiser le code en fonctions
weight: 18
---

## Comment faire :
Dans Fish, vous écrivez une fonction avec le mot-clé `function`, vous lui donnez un nom, et vous terminez par `end`. Voici un exemple simple :

```fish
function hello
    echo "Hello, World!"
end

hello
```

Sortie :
```
Hello, World!
```

Maintenant, faisons-la saluer un utilisateur :

```fish
function greet
    set user (whoami)
    echo "Hey there, $user!"
end

greet
```

Sortie : 
```
Hey there, your_username!
```

Pour la sauvegarder entre les sessions, utilisez `funcsave greet`.

## Exploration plus profonde
Les fonctions de Fish Shell sont comme des mini-scripts — vous pouvez pratiquement tout y mettre. Historiquement, le concept de fonctions dans les scripts shell a sauvé d'innombrables heures de frappe et de débogage répétitifs. Contrairement aux langages de programmation comme Python, les fonctions Shell sont plus une question de commodité que de structure.

Certains shells, comme Bash, utilisent `function` ou juste des accolades. Fish reste fidèle à `function ... end` — clair et lisible. À l'intérieur des fonctions Fish, vous avez tous les avantages : paramètres, variables locales avec `set -l`, et vous pouvez même définir une fonction à l'intérieur d'une autre fonction.

Vous n'aurez pas besoin d'une valeur de retour car Fish n'accorde pas beaucoup d'importance à cela ; la sortie de votre fonction est son retour. Et si vous voulez des fonctions persistantes disponibles pour les sessions futures, n'oubliez pas `funcsave`.

## Voir Aussi
- Le tutoriel fish sur les fonctions : [https://fishshell.com/docs/current/tutorial.html#tut_functions](https://fishshell.com/docs/current/tutorial.html#tut_functions)

### Commandes de fonction
- [function](https://fishshell.com/docs/current/cmds/function.html) — Créer une fonction
- [functions](https://fishshell.com/docs/current/cmds/functions.html) — Afficher ou effacer des fonctions
- [funcsave](https://fishshell.com/docs/current/cmds/funcsave.html) — Enregistrer la définition d'une fonction dans le répertoire de chargement automatique de l'utilisateur
- [funced](https://fishshell.com/docs/current/cmds/funced.html) — Modifier une fonction de manière interactive
