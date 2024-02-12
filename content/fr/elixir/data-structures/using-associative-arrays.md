---
title:                "Utilisation des tableaux associatifs"
aliases:
- /fr/elixir/using-associative-arrays/
date:                  2024-01-30T19:10:55.955119-07:00
model:                 gpt-4-0125-preview
simple_title:         "Utilisation des tableaux associatifs"

tag:                  "Data Structures"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/elixir/using-associative-arrays.md"
changelog:
  - 2024-01-30, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Quoi & Pourquoi ?

Dans Elixir, les tableaux associatifs, appelés Maps, sont des collections de paires clé-valeur où une clé unique pointe vers une valeur. Ils sont extrêmement pratiques pour stocker et récupérer des données à la volée, rendant votre code plus propre et votre vie plus facile.

## Comment faire :

Créer une Map est simple. Vous utilisez la syntaxe `%{}`, comme ceci :

```elixir
my_map = %{"name" => "Alex", "age" => 32}
IO.inspect(my_map)
```

Accéder aux valeurs se fait en utilisant les clés :

```elixir
IO.puts my_map["name"]
```
Sortie : `Alex`

Pour ajouter ou mettre à jour des valeurs, vous pouvez utiliser la fonction `Map.put/3` :

```elixir
updated_map = Map.put(my_map, "location", "NY")
IO.inspect(updated_map)
```
Sortie : `%{"age" => 32, "location" => "NY", "name" => "Alex"}`

Supprimer des clés est tout aussi simple avec `Map.delete/2` :

```elixir
trimmed_map = Map.delete(updated_map, "age")
IO.inspect(trimmed_map)
```
Sortie : `%{"location" => "NY", "name" => "Alex"}`

## Plongée Profonde

Les Maps dans Elixir sont une évolution des anciens types de stockage clé-valeur, comme les Hashes en Ruby ou les Dictionaries en Python. Ils permettent des recherches et des insertions plus efficaces, les rendant un choix privilégié pour la programmation Elixir moderne. Il est important de noter qu'avant les Maps, Elixir utilisait les modules HashDict et Dict, qui sont maintenant dépréciés.

Cependant, pour des scénarios nécessitant des données ordonnées, vous pourriez regarder les listes de mots-clés dans Elixir. Ce sont des listes de tuples, efficaces pour de petites collections mais pas aussi performantes pour de grands ensembles de données comme les Maps.

Gardez à l'esprit que les Maps stockent leurs clés dans une structure "plate", rendant l'accès direct à des valeurs imbriquées un peu délicat. Pour un imbriquage profond, vous pourriez envisager un accès structuré via les fonctions `get_in`, `put_in`, `update_in`, et `get_and_update_in`, qui permettent une approche plus dynamique de la manipulation des données imbriquées.

En somme, alors que les Maps sont votre choix par défaut pour les besoins en tableaux associatifs dans Elixir, le langage offre une riche variété de structures de données pour chaque scénario, vous encourageant à choisir l'outil approprié pour le travail.
