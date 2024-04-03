---
date: 2024-01-20 17:36:29.839999-07:00
description: "Convertir une date en cha\xEEne de caract\xE8res permet de l'afficher\
  \ dans un format lisible pour l'humain. Les d\xE9veloppeurs utilisent cette conversion\
  \ pour\u2026"
lastmod: '2024-03-13T22:44:57.339864-06:00'
model: gpt-4-1106-preview
summary: "Convertir une date en cha\xEEne de caract\xE8res permet de l'afficher dans\
  \ un format lisible pour l'humain."
title: "Conversion d'une date en cha\xEEne de caract\xE8res"
weight: 28
---

## How to:
Elixir utilise le module `Date` pour manipuler les dates. Pour convertir une date en chaîne, l'approche standard est d'utiliser la fonction `to_string/1`.

```elixir
date = ~D[2023-04-12]
date_string = Date.to_string(date)
IO.puts(date_string)
```

Sortie:
```
"2023-04-12"
```

Si vous voulez personnaliser le format, utilisez le module `Timex` (une bibliothèque tierce).

```elixir
{:ok, timex} = Code.ensure_loaded(:timex)
date = ~N[2023-04-12T15:30:00]
date_string = Timex.format!(date, "{YYYY}-{0M}-{0D} {0h}:{0m}:{0s}")
IO.puts(date_string)
```

Sortie:
```
"2023-04-12 15:30:00"
```

## Deep Dive:
Historiquement, Elixir suit les conventions de strftime pour le formatage des chaînes de date/temps, mais la librairie standard n'offre pas directement cette fonctionnalité ; d'où la popularité de `Timex`.

En alternative, vous pouvez aussi utiliser la fonction `inspect/1` pour un débogage rapide, mais cela n'est pas recommandé pour l'affichage utilisateur.

```elixir
date = ~D[2023-04-12]
IO.puts(inspect(date))
```

Sortie:
```
"~D[2023-04-12]"
```

Pour les détails d'implémentation, Elixir gère les dates avec le module `Calendar`, qui supporte différentes calendriers. Le formatage de date personnalisable transforme une structure `Date` ou `NaiveDateTime` en chaîne selon le format spécifié.

## See Also:
- Documentation officielle d'Elixir pour `Date`: https://hexdocs.pm/elixir/Date.html
- GitHub de Timex: https://github.com/bitwalker/timex
- Elixir School sur le traitement de dates: https://elixirschool.com/fr/lessons/essentials/date_time/
