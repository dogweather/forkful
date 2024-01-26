---
title:                "Organisation du code en fonctions"
date:                  2024-01-26T01:10:32.641507-07:00
model:                 gpt-4-1106-preview
simple_title:         "Organisation du code en fonctions"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/fish-shell/organizing-code-into-functions.md"
---

{{< edit_this_page >}}

## Quoi & Pourquoi ?
Organiser le code en fonctions consiste à rassembler des morceaux de script pour réaliser des tâches spécifiques. Nous le faisons parce que cela rend le code plus facile à lire, à tester et à réutiliser — personne ne veut patauger dans un marécage de code enchevêtré.

## Comment faire :
Dans Fish, vous créez une fonction avec le mot-clé `function`, vous lui donnez un nom, et vous terminez par `end`. Voici un exemple simple :

```fish
function hello
    echo "Bonjour, le monde !"
end

hello
```

Sortie :
```
Bonjour, le monde !
```

Maintenant, faisons-la saluer un utilisateur :

```fish
function greet
    set user (whoami)
    echo "Salut, $user !"
end

greet
```

Sortie :
```
Salut, votre_nom_d'utilisateur !
```

Pour la sauvegarder entre les sessions, utilisez `funcsave greet`.

## Plongée en profondeur
Les fonctions de Fish Shell sont comme des mini-scripts — vous pouvez y mettre à peu près n'importe quoi. Historiquement, le concept de fonctions dans les scripts shell a permis d'économiser d'innombrables heures de frappe et de débogage répétitifs. Contrairement aux langages de programmation comme Python, les fonctions Shell sont plus axées sur la commodité que sur la structure.

Certains shells, comme Bash, utilisent `function` ou simplement des accolades. Fish s'en tient à `function ... end` — clair et lisible. À l'intérieur des fonctions Fish, vous disposez de toutes les fonctionnalités : paramètres, variables locales avec `set -l`, et vous pouvez même définir une fonction à l'intérieur d'une autre fonction.

Vous n'aurez pas besoin d'une valeur de `return` parce que Fish n'y accorde pas une grande importance ; la sortie de votre fonction est sa valeur de retour. Et si vous voulez des fonctions persistantes disponibles pour les sessions futures, souvenez-vous de `funcsave`.

## Voir aussi
- Le tutoriel fish sur les fonctions : https://fishshell.com/docs/current/tutorial.html#tut_functions
- La documentation fish pour `function` : https://fishshell.com/docs/current/cmds/function.html
- Un guide complet sur l'écriture de fonctions dans fish : https://fishshell.com/docs/current/index.html#syntax-function