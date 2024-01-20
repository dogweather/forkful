---
title:                "Imprimer la sortie de débogage"
html_title:           "Arduino: Imprimer la sortie de débogage"
simple_title:         "Imprimer la sortie de débogage"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/elixir/printing-debug-output.md"
---

{{< edit_this_page >}}

## Quoi & Pourquoi ?

L'affichage d'informations de débogage, c'est-à-dire l'écriture de données de programme à des fins de vérification. Les programmeurs l'utilisent pour comprendre et corriger les erreurs du code source.

## Comment faire:

Voici comment vous pouvez imprimer des sorties en débogage dans Elixir:

```Elixir
IO.inspect([1, 2, 3])
```

Cela affichera et renverra: `[1, 2, 3]`

Vous pouvez aussi imprimer le débogage avec un message:

```Elixir
IO.inspect([1, 2, 3], label: "debug")
```

À la sortie vous aurez: `debug: [1, 2, 3]`

## Plongée profonde

Elixir est un langage fonctionnel conçu pour créer des applications évolutives et faciles à maintenir. L'impression de debug est une caractéristique qui a été héritée de Erlang, la langue mère d'Elixir.

Il existe des alternatives à `IO.inspect` comme `IO.puts`, mais `IO.puts` n'envoie que sur la sortie standard tandis que `IO.inspect` renvoie également la donnée inspectée.

Dans les coulisses, `IO.inspect` utilise le protocole `Inspect` pour transformer les données en un format lisible. Il est très utile dans les situations où vous avez besoin de voir une représentation plus détaillée des données, comme des tuples, des listes et des maps.

## Voir aussi

Pour plus d'informations, vous pouvez consulter les ressources suivantes:

- [Documentation officielle de Elixir](https://hexdocs.pm/elixir/IO.html#inspect/2)
- [Guide d'introduction Elixir](https://elixir-lang.org/getting-started/debugging.html)
- [Forum Elixir](https://elixirforum.com/)