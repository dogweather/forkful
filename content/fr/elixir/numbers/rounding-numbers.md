---
date: 2024-01-26 03:43:52.819457-07:00
description: "Arrondir des nombres signifie les ajuster \xE0 une valeur proche pour\
  \ la simplicit\xE9 ou pour correspondre \xE0 une certaine pr\xE9cision. C'est utile\
  \ pour am\xE9liorer\u2026"
lastmod: '2024-02-25T18:49:54.205872-07:00'
model: gpt-4-0125-preview
summary: "Arrondir des nombres signifie les ajuster \xE0 une valeur proche pour la\
  \ simplicit\xE9 ou pour correspondre \xE0 une certaine pr\xE9cision. C'est utile\
  \ pour am\xE9liorer\u2026"
title: Arrondir les nombres
---

{{< edit_this_page >}}

## Quoi & Pourquoi ?
Arrondir des nombres signifie les ajuster à une valeur proche pour la simplicité ou pour correspondre à une certaine précision. C'est utile pour améliorer la lisibilité, réduire l'espace de stockage ou répondre à des besoins spécifiques au domaine, comme les calculs d'argent où vous souhaitez arrondir au centime le plus proche.

## Comment faire :
Dans Elixir, vous pouvez utiliser `Float.round/2` pour arrondir un nombre à virgule flottante. Vous pouvez spécifier le nombre de décimales que vous souhaitez conserver. Voici comment cela fonctionne :

```elixir
# Arrondir un nombre sans décimales
Float.round(3.14159) # => 3.0

# Arrondir un nombre à 2 décimales
Float.round(3.14159, 2) # => 3.14

# Arrondir un nombre avec une précision négative au 10 le plus proche
Float.round(123.456, -1) # => 120.0
```

## Exploration en profondeur
Arrondir des nombres est un problème classique en informatique—à tel point que le choix de la stratégie d'arrondi peut impacter les systèmes financiers, les calculs scientifiques et plus encore. `Float.round/2` de Elixir utilise par défaut l'arrondi "à la demi-supérieure", ressemblant à l'arrondi traditionnel enseigné en classe de mathématiques.

Si vous avez besoin d'autres types d'arrondis, Elixir vous permet de créer les vôtres. Considérez, par exemple, l'arrondi "plancher" (toujours vers le bas) ou l'arrondi "plafond" (toujours vers le haut). Vous utiliseriez `Float.floor/1` ou `Float.ceil/1`, respectivement.

```elixir
# Arrondi plancher
Float.floor(3.999) # => 3.0

# Arrondi plafond
Float.ceil(3.001) # => 4.0
```

Ces alternatives aident à adapter l'arrondi aux besoins exacts de votre application, qu'il s'agisse de calculs financiers, de rendu graphique ou d'approximation de données.

## Voir aussi
Pour plus d'informations sur les fonctions d'arrondi de Elixir et les nombres à virgule flottante :

- La documentation officielle de Elixir sur `Float` : https://hexdocs.pm/elixir/Float.html
- La norme IEEE pour l'arithmétique à virgule flottante (IEEE 754) : https://ieeexplore.ieee.org/document/4610935
