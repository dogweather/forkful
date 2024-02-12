---
title:                "Organisation du code en fonctions"
aliases:
- /fr/elixir/organizing-code-into-functions/
date:                  2024-01-26T01:09:43.328748-07:00
model:                 gpt-4-1106-preview
simple_title:         "Organisation du code en fonctions"

tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/elixir/organizing-code-into-functions.md"
---

{{< edit_this_page >}}

## Quoi & Pourquoi ?
Organiser le code en fonctions consiste à regrouper les opérations liées en blocs réutilisables. Nous le faisons pour améliorer la lisibilité et la maintenabilité, réduire la duplication et simplifier les tests.

## Comment faire :
Créons une fonction Elixir simple pour mettre en majuscule les mots :

```elixir
defmodule StringUtils do
  def capitalize_words(sentence) do
    sentence
    |> String.split()
    |> Enum.map(&String.capitalize/1)
    |> Enum.join(" ")
  end
end

IO.puts StringUtils.capitalize_words("hello elixir world")
```
Sortie :
```
Hello Elixir World
```
Ici, nous avons soigneusement emballé la logique de capitalisation des mots dans une fonction appelée `capitalize_words`.

## Plongée approfondie
Dans Elixir, et plus largement dans l'écosystème de la machine virtuelle Erlang, les fonctions sont des citoyens de première classe, héritant de la philosophie de décomposition des problèmes en petits morceaux gérables et isolés. Historiquement, cette approche fonctionnelle trouve ses racines dans le calcul lambda et les Lisps, promouvant la philosophie du code en tant que données.

Les alternatives pour organiser le code peuvent être l'utilisation de macros ou de processus dans Elixir pour des tâches répétitives ou concurrentes, respectivement. En termes de mise en œuvre, les fonctions Elixir peuvent gérer la correspondance de motifs et recevoir différents arguments (arité), leur conférant une polyvalence.

## Voir aussi
- [La documentation officielle d'Elixir sur les fonctions](https://hexdocs.pm/elixir/Kernel.html#functions)
- ["Programming Elixir" de Dave Thomas](https://pragprog.com/titles/elixir16/programming-elixir-1-6/)
