---
title:                "Concaténation de chaînes de caractères"
date:                  2024-01-20T17:34:00.218205-07:00
model:                 gpt-4-1106-preview
simple_title:         "Concaténation de chaînes de caractères"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/bash/concatenating-strings.md"
---

{{< edit_this_page >}}

## What & Why? (Quoi et Pourquoi ?)
Concaténer des chaînes, c'est les coller bout à bout. Les programmeurs le font pour assembler des textes dynamiques, comme créer des messages ou gérer des chemins de fichiers.

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
