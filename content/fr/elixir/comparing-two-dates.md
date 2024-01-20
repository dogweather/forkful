---
title:                "Comparer deux dates"
html_title:           "Clojure: Comparer deux dates"
simple_title:         "Comparer deux dates"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/elixir/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Quoi & Pourquoi ?

Comparer deux dates signifie examiner la différence temporelle entre elles. C'est une compétence essentielle en programmation, utilisée pour tout, de la chronologie des événements à la gestion des délais.

## Comment faire :

Voici comment comparer deux dates en Elixir. Tout d'abord, nous allons créer deux dates :

```elixir
date1 = Date.new(2022, 1, 1)
date2 = Date.new(2023, 1, 1)
```

Ensuite, nous utilisons la fonction `compare` :

```elixir
Date.compare(date1, date2)
```

Le résultat affiche la comparaison entre les deux dates :

```elixir
{:lt, :ok}
```

Ici, `:lt` signifie que `date1` est moins que `date2`. D'autres résultats possibles comprennent `:eq` (égal à) et `:gt` (plus grand que).

## Plongée profonde

Comparaison de dates s'étend au-delà de simples évaluations `<` ou `>`. Historiquement, la gestion du temps et des dates a toujours été un défi dans la programmation, avec des problèmes comme l'effet 2000 (bug de l'an 2000) et l'ajout de jours supplémentaires lors des années bissextiles.

En Elixir, la comparaison de dates utilise le calendrier ISO 8601 par défaut, qui tient compte de ces détails. Cependant, il peut être modifié pour d'autres systèmes si nécessaire.

Alternativement, on peut utiliser des librairies comme "Timex" pour des fonctions approfondies de gestion du temps.

## Voir également

1. Documentation Elixir sur les dates : https://hexdocs.pm/elixir/Date.html
2. Introduction à Elixir pour les débutants : https://elixir-lang.org/getting-started/introduction.html
3. Guide sur l'utilisation de Timex : https://hexdocs.pm/timex/readme.html