---
title:                "Utiliser des expressions régulières"
html_title:           "Elixir: Utiliser des expressions régulières"
simple_title:         "Utiliser des expressions régulières"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/elixir/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Qu'est-ce que c'est et pourquoi les programmeurs l'utilisent-ils?

Les expressions régulières (ou regex) sont une méthode puissante pour rechercher et manipuler des chaînes de caractères en utilisant des motifs spécifiques. Les programmeurs utilisent les regex pour effectuer des tâches telles que la validation des données utilisateur, la recherche de motifs dans un texte ou la manipulation de chaînes de caractères complexes.

## Comment faire:

```Elixir
Regex.match?(~r/\A[a-z]+[0-9]+\z/, "abc123") 
# => true

Regex.replace("Hello, World!", ~r/hello/i, "Bonjour") 
# => "Bonjour, World!"

Regex.split("apple, banana, orange", ~r/, /) 
# => ["apple", "banana", "orange"]
```

## Plongée en profondeur:

Les expressions régulières ont été inventées dans les années 1950 par le mathématicien et informaticien américain Stephen Kleene. En dehors de leur utilisation en programmation, elles sont également largement utilisées dans les éditeurs de texte, les moteurs de recherche et les programmes de traitement de texte.

Il existe plusieurs alternatives aux expressions régulières, telles que les expressions rationnelles et les grammaires régulières. Cependant, les regex sont devenues un outil populaire pour les programmeurs en raison de leur flexibilité et de leur simplicité d'utilisation.

En Elixir, les expressions régulières sont mises en œuvre à l'aide du module Regex et peuvent être utilisées avec les fonctions de correspondance, de remplacement et de division de chaînes de caractères.

## Voir aussi:

- [La documentation officielle sur les expressions régulières en Elixir](https://hexdocs.pm/elixir/Regex.html)
- [Un guide pratique sur l'utilisation des regex en Elixir](https://elixir-lang.org/getting-started/pattern-matching.html#regular-expressions)