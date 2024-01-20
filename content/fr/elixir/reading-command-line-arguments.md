---
title:                "Lecture des arguments de ligne de commande"
html_title:           "Ruby: Lecture des arguments de ligne de commande"
simple_title:         "Lecture des arguments de ligne de commande"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/elixir/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Quoi & Pourquoi?

La lecture d'arguments de ligne de commande est le processus par lequel une application reçoit des paramètres externes au lancement. C'est utile pour personnaliser le comportement d'un programme à l'exécution sans avoir à modifier le code source.

## Comment faire:

En Elixir, nous utilisons `System.argv/0` pour les lire. Voici un exemple de son utilisation:

```elixir
defmodule Main do
  def main() do
    args = System.argv()
    IO.inspect(args)
  end
end
```
Si vous lancez ce programme avec `elixir main.exs arg1 arg2`, l'affichage sera `["arg1", "arg2"]`.

## Approfondissement

Des alternatives existent, chaque langage de programmation ayant sa propre façon de gérer les arguments de ligne de commande. Par exemple, en Python, nous utilisons `sys.argv`.

Historiquement, le concept de lecture d'arguments de ligne de commande existe depuis les premiers jours des systèmes d'exploitation en ligne de commande. Il offre une grande flexibilité dans la manière dont les programmes peuvent être utilisés et combinés.

La fonction `System.argv/0` en Elixir simplement renvoie les arguments lancés avec le script comme une liste de chaînes de caractères. Les arguments sont lus dans l'ordre et sont accessibles immédiatement lors de l'exécution du programme.

## Voir Aussi

Voici quelques liens pour approfondir vos connaissances sur le sujet:

1. [System.argv — Elixir v1.11.3](https://hexdocs.pm/elixir/System.html#argv/0)