---
date: 2024-01-20 17:34:00.218205-07:00
description: "How to: (Comment faire :) Concat\xE9ner en Bash est simple. Utilisez\
  \ des guillemets, des accolades ou mettez juste les cha\xEEnes c\xF4te \xE0 c\xF4\
  te. Voici des\u2026"
lastmod: '2024-04-05T21:53:59.444974-06:00'
model: gpt-4-1106-preview
summary: "(Comment faire :) Concat\xE9ner en Bash est simple."
title: "Concat\xE9nation de cha\xEEnes de caract\xE8res"
weight: 3
---

## How to: (Comment faire :)
Concaténer en Bash est simple. Utilisez des guillemets, des accolades ou mettez juste les chaînes côte à côte. Voici des exemples :

```Bash
# Concaténation simple
greeting="Salut, "
name="Jean!"
welcome_message=$greeting$name
echo $welcome_message  # Affiche "Salut, Jean!"

# Avec des accolades pour plus de clarté
path="/usr/"
subfolder="local"
full_path="${path}${subfolder}"
echo $full_path  # Affiche "/usr/local"

# Sans espaces
first_part="Bonjour"
second_part="tout le monde"
combined="$first_part$second_part"
echo $combined  # Affiche "Bonjourtout le monde"
```

## Deep Dive (Plongée en profondeur)
Concaténer des chaînes n'a pas évolué depuis les débuts du shell. C'est une opération fondamentale et simple. Historiquement, `echo` est souvent utilisé pour afficher des chaînes concaténées. Des alternatives existent, comme utiliser `printf` pour plus de contrôle sur le formatage, ou joindre des chaînes avec `paste` pour des fichiers.

Bash ne distingue pas entre variables de type chaîne ou autre, ce qui simplifie la concaténation. Cependant, il faut être prudent avec les espaces, qui peuvent être interprétés comme des séparateurs d'argument.

Pour les grosses opérations, on peut constater un impact sur les performances avec les méthodes simples de concaténation. Des outils comme `awk` ou `sed` peuvent être plus efficaces pour traiter de grandes quantités de texte.

## See Also (Voir aussi)
- La page man Bash pour les variables: `man bash` et recherchez `/Parameter Expansion`.
- Guide avancé de script Bash: http://tldp.org/LDP/abs/html/
- Discussion détaillée de la concaténation: https://www.gnu.org/software/bash/manual/bash.html#Brace-Expansion
