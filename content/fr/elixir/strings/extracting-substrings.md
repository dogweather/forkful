---
date: 2024-01-20 17:45:24.292687-07:00
description: "How to: (Comment faire : ) Historiquement, extraire des sous-cha\xEE\
  nes est important pour la manipulation de texte (pensez au C avec `strncpy`), mais\u2026"
lastmod: '2024-04-05T22:51:11.432764-06:00'
model: gpt-4-1106-preview
summary: "(Comment faire : ) Historiquement, extraire des sous-cha\xEEnes est important\
  \ pour la manipulation de texte (pensez au C avec `strncpy`), mais Elixir, n\xE9\
  \ en 2011, rend le processus plus facile et plus s\xFBr sans pointer de manipulation\
  \ directe. Alternativement, on peut utiliser des r\xE9gex avec `Regex.scan/2`, mais\
  \ c'est plus lourd. La cl\xE9 est `String`, un module avec des fonctions optimis\xE9\
  es qui s'occupent de l'UTF-8 automatiquement, donc pas de souci pour l'encodage."
title: "Extraction de sous-cha\xEEnes"
weight: 6
---

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
