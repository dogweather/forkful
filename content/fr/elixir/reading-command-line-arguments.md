---
title:                "Lecture des arguments de ligne de commande"
html_title:           "Elixir: Lecture des arguments de ligne de commande"
simple_title:         "Lecture des arguments de ligne de commande"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/elixir/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Pourquoi

Êtes-vous fatigué de toujours avoir à modifier votre code pour tester différentes valeurs d'entrée? Ou peut-être venez-vous de découvrir Elixir et voulez en savoir plus sur la lecture des arguments de ligne de commande? Dans cet article, nous allons explorer comment lire efficacement les arguments de ligne de commande en utilisant Elixir, afin de faciliter votre expérience de développement.

## Comment faire

Pour commencer, nous allons créer un projet Elixir simple en utilisant Mix, l'outil de build d'Elixir. Ouvrez votre terminal et exécutez les commandes suivantes:

```
mix new cli_args
cd cli_args
```

Cela créera un nouveau dossier `cli_args` contenant un fichier `mix.exs` et un dossier `lib`. Nous allons travailler dans le dossier `lib` pour cet exemple.

Maintenant, nous allons ajouter une fonction `setup_cli` à notre module `CliArgs` qui va lire les arguments de ligne de commande. Pour cela, nous allons utiliser les modules `System` et `OptionParser` d'Elixir. Voici à quoi cela ressemble:

```
defmodule CliArgs do
  def setup_cli do
    system_args = System.argv()
    args = OptionParser.parse(system_args)

    # Faites quelque chose avec les arguments ici...
  end
end
```

La fonction `System.argv()` nous donne une liste des arguments passés à notre script lors de son exécution. Nous utilisons ensuite `OptionParser` pour parser ces arguments et les transformer en un format plus gérable.

Pour référence, voici la structure de notre projet jusqu'à présent:

```
.
├── lib
│   └── cli_args.ex
└── mix.exs
```

Enfin, nous allons exécuter notre fonction `setup_cli` en l'appelant depuis `mix`. Dans votre terminal, exécutez la commande suivante:

```
mix run -e "CliArgs.setup_cli"
```

Voici un exemple de sortie pour cette commande avec quelques arguments:

```
$ mix run -e "CliArgs.setup_cli" --message "Hello World" --name "John"
[
  message: "Hello World",
  name: "John"
]
```

Et voilà! Nous avons réussi à lire efficacement les arguments de ligne de commande en utilisant Elixir.

## Plongée profonde

Maintenant que nous avons vu un exemple fonctionnel de lecture d'arguments de ligne de commande en utilisant Elixir, explorons quelques astuces supplémentaires pour en tirer le meilleur parti.

Tout d'abord, la fonction `OptionParser.parse` prend en option un deuxième argument, qui spécifie la structure des arguments attendus. Cela peut être utile si vous avez besoin d'arguments de types spécifiques ou si vous souhaitez spécifier des arguments obligatoires. Vous pouvez en savoir plus sur les options disponibles dans la documentation officielle d'Elixir pour `OptionParser`.

Deuxièmement, vous pouvez également utiliser `OptionParser` pour afficher une aide pour votre script en cas de mauvaise utilisation. Cela peut être fait en ajoutant une clause `else` à votre fonction `setup_cli` qui appelle `OptionParser.help` avec un message d'erreur approprié.

Enfin, si vous travaillez avec des arguments plus complexes, vous pouvez utiliser `OptionParser.parse!`, qui lève une erreur si l'un des arguments ne correspond pas à la structure spécifiée. Cela peut vous aider à identifier rapidement les problèmes avec vos arguments lors du développement.

## Voir aussi

- [Documentation officielle d'Elixir sur OptionParser](https://hexdocs.pm/elixir/OptionParser.html)
- [Exemple de projet Elixir utilisant OptionParser](https://github.com/elixir-lang/elixir/blob/master/examples/option_parser.ex)