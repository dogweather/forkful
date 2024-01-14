---
title:                "Fish Shell: Trouver la longueur d'une chaîne de caractères"
programming_language: "Fish Shell"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/fish-shell/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

# Pourquoi

Trouver la longueur d'une chaîne de caractères est une tâche courante en programmation. Que ce soit pour vérifier la validité des données saisies ou pour manipuler des chaînes, connaître la longueur exacte d'une chaîne est essentiel. Dans cet article, nous allons vous montrer comment le faire facilement en utilisant Fish Shell.

## Comment faire

Pour trouver la longueur d'une chaîne en utilisant Fish Shell, nous allons utiliser la commande `string length`. Voici un exemple de code avec une chaîne de caractères 'Bonjour' :

```
Fish Shell
string length 'Bonjour'
```

Lorsque vous exécutez ce code, vous obtiendrez une sortie avec le nombre de caractères de la chaîne donnée :

```
7
```

Vous pouvez également utiliser cette commande pour trouver la longueur de variables dans votre script :

```
set string 'Bonjour'
string length $string
```

La sortie de ce code sera également 7.

## Plongée en profondeur

Si vous souhaitez en savoir plus sur la façon dont la commande `string length` fonctionne, voici quelques informations supplémentaires. Cette commande se base sur la variable interne `$argv` qui stocke les arguments fournis à Fish Shell. Elle compte simplement le nombre de caractères dans cette variable et renvoie le résultat.

Il est également important de noter que cette commande ne prend pas en compte les caractères spéciaux et les espaces blancs dans la chaîne de caractères.

# Voir aussi

- [Documentation officielle Fish Shell](https://fishshell.com/docs/current)
- [Tutoriel Fish Shell pour débutants](https://medium.com/@sergiomatta/fish-shell-tutorial-2-variables-and-functions-126e518a3a6f)
- [Exemples de code Fish Shell](https://github.com/fish-shell/fish-shell/tree/master/share/examples)