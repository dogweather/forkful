---
title:                "Interpoler une chaîne de caractères"
html_title:           "Elixir: Interpoler une chaîne de caractères"
simple_title:         "Interpoler une chaîne de caractères"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/elixir/interpolating-a-string.md"
---

{{< edit_this_page >}}

## Qu'est-ce que c'est et pourquoi faire?

Interpoler une chaîne consiste à inclure des expressions ou variables dans une chaîne de caractères. Cela permet aux programmeurs d'ajouter facilement des valeurs dynamiques à une chaîne sans avoir à manipuler manuellement chaque partie de la chaîne.

Les programmeurs utilisent souvent l'interpolation de chaînes pour générer des messages d'erreur ou pour formater des données affichées à l'utilisateur.

## Comment faire:

```elixir
variable = "Elixir"
"Il est temps d'apprendre #{variable}!"
```

Le code ci-dessus va retourner "Il est temps d'apprendre Elixir!" lorsque vous exécutez le programme. Les expressions ou variables incluses entre ```#{ }``` seront évaluées et remplacées par leur valeur respective dans la chaîne.

```elixir
nom = "Jean"
age = 30
"#{nom} a #{age} ans."
```

Ce code donnera "Jean a 30 ans." en sortie.

## Plongée en profondeur:

L'interpolation de chaînes est une fonctionnalité couramment utilisée dans de nombreux langages de programmation, tels que Ruby, Python et bien sûr, Elixir.

Certaines alternatives à l'interpolation de chaînes incluent la concaténation de chaînes et l'utilisation de fonctions de formatage de chaînes spécifiques. Cependant, l'interpolation de chaînes permet une syntaxe plus concise et plus lisible.

En termes d'implémentation, l'interpolation de chaînes est généralement réalisée en utilisant des fonctions spéciales dédiées à cette tâche. Dans Elixir, la fonction ```to_string``` est utilisée pour convertir les valeurs en chaînes et ```<>``` pour concaténer des chaînes.

## Voir aussi:

Pour plus d'informations sur l'interpolation de chaînes en Elixir, consultez la documentation officielle sur les chaînes: https://hexdocs.pm/elixir/String.html#interpolation. Vous pouvez également explorer d'autres fonctionnalités utiles de manipulation de chaînes en Elixir, telles que la découpe de chaînes et la substitution de motifs.