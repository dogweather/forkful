---
date: 2024-01-26 04:39:06.475238-07:00
description: "Les nombres complexes ont une partie r\xE9elle et une partie imaginaire\
  \ (comme `3 + 4i`). Ils sont utilis\xE9s en ing\xE9nierie, en physique et pour certains\u2026"
lastmod: 2024-02-19 22:05:16.217592
model: gpt-4-0125-preview
summary: "Les nombres complexes ont une partie r\xE9elle et une partie imaginaire\
  \ (comme `3 + 4i`). Ils sont utilis\xE9s en ing\xE9nierie, en physique et pour certains\u2026"
title: Manipulation des nombres complexes
---

{{< edit_this_page >}}

## Quoi et pourquoi ?
Les nombres complexes ont une partie réelle et une partie imaginaire (comme `3 + 4i`). Ils sont utilisés en ingénierie, en physique et pour certains problèmes informatiques. Les programmeurs les utilisent pour les simulations, le traitement des signaux et la résolution efficace de certains types de problèmes mathématiques.

## Comment faire :
Elixir n'a pas de nombres complexes intégrés, donc nous devons créer les nôtres ou utiliser une bibliothèque, comme `ComplexNum`. Voici un rapide exemple avec une bibliothèque :

```elixir
# En supposant que vous avez installé ComplexNum
defmodule ComplexMath do
  import ComplexNum

  def add(a, b) do
    ComplexNum.add(a, b)
  end
end

# Créer des nombres complexes et les additionner
c1 = {3, 4}   # représente 3 + 4i
c2 = {2, -3}  # représente 2 - 3i
resultat = ComplexMath.add(c1, c2)
IO.puts "Le résultat est : #{inspect(resultat)}"
```

Cela afficherait :
```
Le résultat est : {5, 1}
```

Cela signifie que la somme de `3 + 4i` et `2 - 3i` est `5 + 1i`.

## Plongée profonde
Les nombres complexes sont apparus dans l'histoire parce que les bon vieux nombres ne pouvaient pas gérer les racines carrées de négatifs. Ce n'est qu'au 17e siècle qu'ils ont été pris au sérieux, grâce à des mathématiciens comme René Descartes et Gerolamo Cardano.

Dans Elixir, vous utilisez souvent des tuples comme `{3, 4}` pour les nombres complexes, ou utilisez une librairie dédiée pour éviter de réinventer la roue. Les bibliothèques sont généralement meilleures - elles gèrent les détails comme la multiplication et la division, qui deviennent compliquées à cause de l'unité imaginaire 'i' (pour info : `i` au carré égale `-1`).

## Voir aussi
Consultez ces ressources :
- [Bibliothèque ComplexNum](https://hex.pm/packages/complex_num) pour le gestionnaire de paquets d'Elixir, Hex.
- [Elixir School](https://elixirschool.com/en/), pour des sujets et exercices avancés sur Elixir.
- [Erlang -- math Module](http://erlang.org/doc/man/math.html), que Elixir utilise sous le capot, pour d'autres besoins mathématiques.
