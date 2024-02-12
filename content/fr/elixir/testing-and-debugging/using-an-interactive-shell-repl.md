---
title:                "Utilisation d'une console interactive (REPL)"
date:                  2024-01-26T04:12:54.443784-07:00
model:                 gpt-4-0125-preview
simple_title:         "Utilisation d'une console interactive (REPL)"

tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/elixir/using-an-interactive-shell-repl.md"
---

{{< edit_this_page >}}

## Quoi et pourquoi ?
Un shell interactif, ou REPL (Read-Eval-Print Loop), vous permet d'essayer des extraits de code en temps réel. Les programmeurs Elixir utilisent le REPL, appelé IEx (Interactive Elixir), pour expérimenter, déboguer, et apprendre le langage.

## Comment faire :
Pour lancer IEx, ouvrez votre terminal et tapez `iex`. Voici un avant-goût :

```Elixir
iex> name = "Elixir Programmer"
"Elixir Programmer"
iex> String.length(name)
17
iex> Enum.map([1, 2, 3], fn num -> num * 3 end)
[3, 6, 9]
```

La sortie devrait montrer l'assignation de variables, les résultats des fonctions, et une fonction anonyme au travail.

## Plongée en profondeur
Le shell IEx fait partie d'Elixir depuis ses premiers jours. José Valim, le créateur d'Elixir, s'est inspiré des shells interactifs d'autres langages comme le `python` de Python et le `irb` de Ruby. Bien qu'IEx partage de nombreuses caractéristiques avec ces derniers, il est conçu pour gérer la nature concurrente d'Elixir et est entièrement intégré aux capacités de la VM Erlang.

Les alternatives à IEx dans l'écosystème Erlang incluent `erl`, le shell Erlang. Mais IEx offre un environnement plus convivial pour Elixir, avec des fonctionnalités telles que l'auto-complétion complète, l'historique, et des assistants.

Le REPL IEx est plus qu'un terrain de jeu ; il peut se connecter de manière transparente à un système en cours d'exécution. Cela est crucial pour le débogage d'applications en direct. L'implémentation sous-jacente repose sur le BEAM (la VM Erlang), garantissant que des fonctionnalités comme l'échange de code à chaud sont prises en charge directement dans le shell.

## Voir aussi
Consultez ces ressources pour approfondir vos connaissances :

- [Documentation d'IEx d'Elixir](https://hexdocs.pm/iex/IEx.html)
- [Elixir Interactif (IEx) - Le Shell Elixir](https://elixir-lang.org/getting-started/introduction.html#interactive-elixir)
- [Documentation d'`erl` d'Erlang](http://erlang.org/doc/man/erl.html)
- [Apprendre le Shell Interactif d’Elixir](https://elixirschool.com/en/lessons/basics/iex_helpers/)