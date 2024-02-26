---
date: 2024-01-26 01:09:43.328748-07:00
description: "Organiser le code en fonctions consiste \xE0 regrouper les op\xE9rations\
  \ li\xE9es en blocs r\xE9utilisables. Nous le faisons pour am\xE9liorer la lisibilit\xE9\
  \ et la\u2026"
lastmod: '2024-02-25T18:49:54.216379-07:00'
model: gpt-4-1106-preview
summary: "Organiser le code en fonctions consiste \xE0 regrouper les op\xE9rations\
  \ li\xE9es en blocs r\xE9utilisables. Nous le faisons pour am\xE9liorer la lisibilit\xE9\
  \ et la\u2026"
title: Organisation du code en fonctions
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
