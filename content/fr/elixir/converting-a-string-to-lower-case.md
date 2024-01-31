---
title:                "Conversion d'une chaîne de caractères en minuscules"
date:                  2024-01-20T17:38:13.861824-07:00
model:                 gpt-4-1106-preview
simple_title:         "Conversion d'une chaîne de caractères en minuscules"

category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/elixir/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## What & Why?
Convertissez une chaîne de caractères en minuscules pour unifier le format des données textuelles. Les programmeurs le font souvent pour comparer des chaînes de manière insensible à la casse ou pour standardiser les entrées utilisateurs.

## How to:
Elixir rend la conversion en minuscules simple avec la fonction `String.downcase/1`. 

```elixir
iex> ma_chaine = "Bonjour, LE MONDE!"
iex> String.downcase(ma_chaine)
"bonjour, le monde!"
```

Si vous travaillez avec des chaînes multilingues:

```elixir
iex> ma_chaine = "ĞÜNAYDIN!"
iex> String.downcase(ma_chaine, :turkish)
"ğünaydın!"
```

Observez comment le `I` majuscule se transforme correctement dans le contexte turc.

## Deep Dive
Convertir des chaînes de caractères en minuscules est une pratique courante, vieille comme l'informatique. Dans le passé, des méthodes plus simples étaient utilisées, souvent limitées à l'ASCII. Cela signifie que seuls les caractères anglais étaient pris en charge, ignorant d'autres langues.

Avec Elixir, les choses sont différentes. Elixir utilise UTF-8 par défaut, gérant ainsi les textes de toutes les langues. La mise en minuscules est plus complexe que de simplement changer les codes de caractères A-Z; il faut prendre en compte les contextes linguistiques, comme le `i` majuscule en turc cité plus haut.

Une alternative brute serait d'utiliser une boucle pour parcourir chaque caractère ou utiliser des expressions régulières pour remplacer les majuscules par des minuscules. Mais ces méthodes ne gèrent pas les cas particuliers multilingues.

Sous le capot, `String.downcase/2` d'Elixir utilise Unicode's Common Locale Data Repository (CLDR) pour s'assurer que chaque caractère est correctement transformé selon les règles de la langue spécifiée.

## See Also
Pour explorer davantage les fonctions de manipulation de chaînes dans Elixir:

- [`String`](https://hexdocs.pm/elixir/String.html) module in Elixir's documentation
- [Unicode's CLDR](http://cldr.unicode.org/) pour les détails sur les règles de conversion de caractères
- [`elixir-lang/elixir`](https://github.com/elixir-lang/elixir) sur GitHub pour voir comment les fonctions de chaînes sont implémentées
