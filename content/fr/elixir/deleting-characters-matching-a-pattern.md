---
title:                "Suppression de caractères correspondant à un motif"
aliases:
- fr/elixir/deleting-characters-matching-a-pattern.md
date:                  2024-01-20T17:41:49.390394-07:00
model:                 gpt-4-1106-preview
simple_title:         "Suppression de caractères correspondant à un motif"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/elixir/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## What & Why? (Quoi et Pourquoi ?)
Supprimer des caractères selon un modèle (pattern) c'est enlever des éléments d'une chaîne qui correspondent à un critère précis. Les développeurs le font pour nettoyer des données, valider des entrées ou manipuler du texte de manière contrôlée.

## How to: (Comment faire :)

Utilisons la fonction `String.replace/3` pour supprimer les caractères qui correspondent à un modèle défini par une expression régulière (regex).

```elixir
defmodule Cleaner do
  def delete_pattern(str, pattern) do
    String.replace(str, ~r/#{pattern}/, "")
  end
end

IO.puts Cleaner.delete_pattern("Bonjour1 les2 programmeurs3!", "\\d")
```

```  
Bonjour les programmeurs!
```

Dans cet exemple, le chiffre `\\d` est le modèle qui identifie tous les chiffres dans la chaîne de caractères, et ils sont supprimés.

## Deep Dive (Plongée en profondeur)

Historiquement, manipuler des chaînes de caractères a toujours été une partie cruciale de la programmation. En Elixir, c'est particulièrement vrai puisque le langage a été construit pour gérer des systèmes distribués et souvent, cela implique de traiter des données textuelles venant de diverses sources. 

Les alternatives à `String.replace/3` incluent l'utilisation de listes de compréhension ou de fonctions dans le module `Enum` pour filtrer caractère par caractère. Cependant, les expressions régulières offrent une solution concise et puissante pour des correspondances complexes.

En interne, Elixir utilise l'ingénierie de Erlang et les bibliothèques pour traiter les expressions régulières, optimisées pour l'efficacité et la vitesse.

## See Also (Voir également)

- Documentation Elixir sur `String.replace/3`: https://hexdocs.pm/elixir/String.html#replace/4
- Guide d'introduction aux expressions régulières en Elixir : https://elixir-lang.org/getting-started/pattern-matching.html
- Fiche récapitulative des expressions régulières (Regex Cheat Sheet) : https://www.debuggex.com/cheatsheet/regex/pcre
