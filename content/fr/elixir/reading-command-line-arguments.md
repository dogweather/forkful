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

## Quoi & Pourquoi?
La lecture des arguments de ligne de commande est le fait de récupérer et de traiter les informations saisies par un utilisateur dans la ligne de commande lors de l'exécution d'un programme. Les programmeurs le font pour permettre à leurs programmes d'interagir avec les utilisateurs et de prendre en compte leurs entrées de manière dynamique.

## Comment faire:
La lecture des arguments de ligne de commande peut être réalisée en utilisant la fonction `System.argv/0` en Elixir. Voici un exemple de code montrant comment récupérer et afficher les arguments saisis par l'utilisateur en utilisant cette fonction:

```
args = System.argv()
IO.puts(args)
```

En exécutant le programme en ligne de commande en utilisant la commande `elixir`, vous pouvez entrer des arguments séparés par des espaces et les voir s'afficher dans la sortie.

```
elixir mon_programme.exs argument1 argument2 argument3
```
Sortie: `[argument1, argument2, argument3]`

## Plongée en profondeur:
La lecture des arguments de ligne de commande est couramment utilisée pour créer des scripts et des programmes en ligne de commande en Elixir. Cependant, il existe d'autres alternatives comme l'utilisation de modules tels que `OptionParser` ou en utilisant des frameworks de ligne de commande comme `Escript`.

La fonction `System.argv/0` utilise l'argument `process.argv` de l'Erlang Virtual Machine (VM) pour récupérer les arguments saisis par l'utilisateur. La VM stocke ces arguments dans une liste sous forme de chaînes de caractères. Il est donc important de les convertir en types de données appropriés avant de les utiliser dans votre programme.

## Voir aussi:
- Documentation officielle pour `System.argv/0`: https://hexdocs.pm/elixir/System.html#argv/0
- Article sur l'utilisation de modules pour la lecture des arguments de ligne de commande en Elixir: https://medium.com/red-ventures-br-tech/reading-command-line-arguments-in-elixir-fd6bbb3204d8