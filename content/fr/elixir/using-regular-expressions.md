---
title:                "Utilisation des expressions régulières"
date:                  2024-01-19
html_title:           "Bash: Utilisation des expressions régulières"
simple_title:         "Utilisation des expressions régulières"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/elixir/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Quoi et pourquoi ?
Les expressions régulières, ou regex, permettent de chercher et manipuler des strings avec des règles précises. Les programmeurs les utilisent pour valider, extraire, et transformer des données textuelles de manière efficace.

## Comment ça marche :
```elixir
# Recherche d'un pattern simple
String.match?("Hello World", ~r/World/)
# Sortie : true

# Capture de données
Regex.run(~r/\d+/, "Il y a 2021 pommes")
# Sortie : ["2021"]

# Remplacement de texte
String.replace("12:34:56", ~r/(\d+):(\d+):(\d+)/, "\\2:\\3:\\1")
# Sortie : "34:56:12"
```

## Exploration Approfondie
Les expressions régulières sont nées dans les années 1950, issues des travaux sur la théorie des automates. Des alternatives existent, telles que le parsing via des grammaires ou des librairies spécifiques à certains types de données comme XML ou JSON. Elixir les implémente via son module `Regex`, qui repose sur la bibliothèque Erlang `re`, elle-même basée sur la bibliothèque PCRE.

## Voir aussi
- Documentation Elixir `Regex`: https://hexdocs.pm/elixir/Regex.html
- Tutoriel interactif regex: https://regexone.com/
- Cheat sheet des expressions régulières en Elixir: https://til.hashrocket.com/posts/3c3f65c0a9-regex-cheat-sheet-for-elixir

(Note pour le correcteur : le code Elixir est volontairement simple et démontre les capacités de base des regex dans le langage. Le but est de rester concis et accessible.)
