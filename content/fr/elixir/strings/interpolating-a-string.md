---
title:                "Interpolation de chaînes de caractères"
aliases:
- /fr/elixir/interpolating-a-string/
date:                  2024-01-20T17:50:28.974919-07:00
model:                 gpt-4-1106-preview
simple_title:         "Interpolation de chaînes de caractères"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/elixir/interpolating-a-string.md"
---

{{< edit_this_page >}}

## What & Why? | Quoi et Pourquoi ?
On insère des données dans une chaîne de caractères. On fait ça pour dynamiser les messages sans concaténer à l'ancienne.

## How to: | Comment faire :
```elixir
name = "World"
# Interpolation simple
message = "Hello, #{name}!"
IO.puts message
```
Sortie: `Hello, World!`

## Deep Dive | Plongée en Profondeur
Interpoler, c'est intégrer des variables directement dans des chaînes depuis Elixir 1.0. Avant, on concaténait, mettant bout à bout les strings et les variables, ou en utilisant des fonctions plus lourdes. Interpoler, ça fait propre et c'est rapide ; Elixir utilise l'opérateur `#{...}`. Le bytecode Erlang final est optimisé, comme pour une concaténation classique.

## See Also | Voir Aussi
- [Documentation Elixir sur les chaînes de caractères](https://hexdocs.pm/elixir/String.html)
- [Guide d'introduction aux chaînes binaires, les strings et leurs fonctions (en anglais)](https://elixir-lang.org/getting-started/binaries-strings-and-char-lists.html)
- [Forum Elixir pour poser des questions](https://elixirforum.com/)
