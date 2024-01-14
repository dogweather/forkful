---
title:                "Gleam: Lecture des arguments de ligne de commande"
simple_title:         "Lecture des arguments de ligne de commande"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/gleam/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Pourquoi

Si vous êtes un développeur qui aime travailler avec la ligne de commande, alors vous savez que la manipulation des arguments en ligne de commande est un aspect important de la programmation. Lire les arguments en ligne de commande peut vous aider à personnaliser l'exécution de votre programme en fonction des entrées de l'utilisateur. Dans cet article, nous allons vous montrer comment lire les arguments en ligne de commande en utilisant Gleam.

## Comment faire

Pour lire les arguments en ligne de commande en utilisant Gleam, nous allons utiliser la fonction `gleam/program/args.get()` qui retourne une liste de valeurs. Dans l'exemple ci-dessous, nous allons afficher tous les arguments passés lors de l'exécution du programme :

```
Gleam program args.get()
|> io.format("%?", "/"println)
```

Si nous exécutons ce programme avec `gleam run program.gleam un deux trois`, la sortie sera `["un", "deux", "trois"]`.

## Plongée en profondeur

Il est important de noter que les arguments en ligne de commande sont traités comme des chaînes de caractères par Gleam. Cela signifie que si vous voulez les utiliser comme des entiers ou des booléens, vous devrez les convertir en utilisant des fonctions telles que `int.from_string()` ou `bool.from_string()`. De plus, il est possible de spécifier des arguments avec des drapeaux en utilisant le préfixe `-` ou `--` avant le nom du drapeau. Dans cet exemple, nous allons afficher la valeur du drapeau `--message` :

```
Gleam program args.get()
|> Map.get("message")
|> option.unwrap_or_else(|| "NO MESSAGE")
|> io.format("The message is: %?", "/", println)
```

Si nous exécutons ce programme avec `gleam run program.gleam --message "Hello World!"`, la sortie sera `The message is: Hello World!`.

## Voir aussi

- La documentation officielle de Gleam sur la manipulation des arguments en ligne de commande : https://gleam.run/book/basics/block-arguments.html
- Un tutoriel en français pour débuter avec Gleam : https://dev.to/csstoltenberg/commencer-avec-gleam-2fig
- Un guide en ligne de Gleam pour apprendre les bases : https://gleam.run/