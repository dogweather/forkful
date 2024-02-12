---
title:                "Extraction de sous-chaînes"
date:                  2024-01-20T17:45:24.292687-07:00
model:                 gpt-4-1106-preview
simple_title:         "Extraction de sous-chaînes"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/elixir/extracting-substrings.md"
---

{{< edit_this_page >}}

## What & Why? (Quoi et Pourquoi ?)
Extraire des sous-chaînes, c'est prendre des morceaux spécifiques d'une chaîne de caractères. Pourquoi ? Pour analyser des données, valider des entrées, ou simplement manipuler du texte.

## How to: (Comment faire : )
```elixir
# Extraire une sous-chaîne avec String.slice/3
original = "Salut le monde !"
substring = String.slice(original, 0, 5)
IO.puts(substring) # "Salut"

# Utilisation des indices négatifs avec String.slice/2
backwards_slice = String.slice(original, -7..-2)
IO.puts(backwards_slice) # "monde"

# Trouver et extraire une sous-chaîne avec String.split/2 et hd/1
parts = String.split(original, " ")
first_word = hd(parts)
IO.puts(first_word) # "Salut"
```

## Deep Dive (Plongée en profondeur)
Historiquement, extraire des sous-chaînes est important pour la manipulation de texte (pensez au C avec `strncpy`), mais Elixir, né en 2011, rend le processus plus facile et plus sûr sans pointer de manipulation directe. Alternativement, on peut utiliser des régex avec `Regex.scan/2`, mais c'est plus lourd. La clé est `String`, un module avec des fonctions optimisées qui s'occupent de l'UTF-8 automatiquement, donc pas de souci pour l'encodage.

## See Also (Voir aussi)
- Elixir `String` module documentation: https://hexdocs.pm/elixir/String.html
- Elixir Getting Started Guide: https://elixir-lang.org/getting-started/basic-types.html
- Joe Armstrong's book "Programming Erlang: Software for a Concurrent World" for historical insight into Erlang, the mother tongue of Elixir.