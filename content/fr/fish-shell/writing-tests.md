---
title:                "Fish Shell: Écriture de tests"
simple_title:         "Écriture de tests"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/fish-shell/writing-tests.md"
---

{{< edit_this_page >}}

# Pourquoi écrire des tests avec Fish Shell?

Ecrire des tests est une pratique essentielle pour les développeurs afin de s'assurer que leur code fonctionne correctement et de détecter d'éventuelles erreurs. En utilisant Fish Shell, vous pouvez facilement créer et exécuter des tests pour votre code, ce qui vous aidera à améliorer la qualité de vos programmes.

## Comment procéder?

Pour écrire des tests avec Fish Shell, vous pouvez utiliser la commande `test` suivie de l'expression à tester et du résultat attendu. Par exemple:

```
Fish Shell> test 1 -eq 1
```

Cela va tester si l'expression `1 -eq 1` est égale à `true` et afficher le résultat.

Vous pouvez également utiliser les commandes `contains` et `matches` pour tester si une chaîne de caractères contient ou correspond à une autre chaîne. Par exemple:

```
Fish Shell> contains "Hello World" "Hello"
```

Ceci va tester si la chaîne "Hello" est incluse dans la chaîne "Hello World".

Pour tester des fonctions, vous pouvez utiliser la commande `set -x` pour afficher les variables et `echo` pour vérifier les résultats. Voici un exemple:

```
Fish Shell> function add_numbers
            > set -x result (set --local a $argv[1]; set --local b $argv[2]; echo $a + $b)
            > test (add_numbers 2 3) -eq 5
```

Ce test va vérifier si la fonction `add_numbers` retourne bien la somme de ses deux arguments.

## Plongée en profondeur

En plus des commandes mentionnées précédemment, Fish Shell propose d'autres fonctionnalités intéressantes pour écrire des tests. Par exemple, vous pouvez utiliser la commande `begin` pour exécuter un bloc de code et vérifier si aucune erreur n'a été signalée, en utilisant la commande `status`. Voici un exemple:

```
Fish Shell> begin
            > echo "Running some code..."
            > exit 0
            > end
            > test (status 0)
```

Vous pouvez également utiliser des opérateurs logiques comme `and` et `or` pour combiner plusieurs tests dans une même commande. Par exemple:

```
Fish Shell> test (1 -eq 1) and (2 -eq 2)
```

Ceci va tester si les deux expressions sont égales à `true`.

## Voir aussi

- [Documentation officielle de Fish Shell](https://fishshell.com/docs/current/cmds/test.html)
- [Guide de l'utilisateur pour les tests en Fish Shell](https://github.com/fish-shell/fish-shell/wiki/Writing-Test-Functions)
- [Exemples de tests en Fish Shell](https://github.com/fish-shell/fish-shell/tree/master/test)