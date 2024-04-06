---
date: 2024-01-20 17:38:33.834595-07:00
description: "How to: (Comment faire :) Historiquement, la gestion des majuscules\
  \ et des minuscules a \xE9t\xE9 importante pour la compatibilit\xE9 et le tri. La\
  \ commande `tr`\u2026"
lastmod: '2024-04-05T22:51:12.170544-06:00'
model: gpt-4-1106-preview
summary: "(Comment faire :) Historiquement, la gestion des majuscules et des minuscules\
  \ a \xE9t\xE9 importante pour la compatibilit\xE9 et le tri."
title: "Conversion d'une cha\xEEne de caract\xE8res en minuscules"
weight: 4
---

## How to: (Comment faire :)
```Fish Shell
# Convertir une chaîne en minuscules
echo "FISH SHELL est Cool!" | tr '[:upper:]' '[:lower:]'
# Sortie: fish shell est cool!
```

```Fish Shell
# Utilisation d'une fonction native Fish pour une variable
set phrase "FISH SHELL Est Génial!"
string tolower -- $phrase
# Sortie: fish shell est génial!
```

## Deep Dive (Plongée Profonde)
Historiquement, la gestion des majuscules et des minuscules a été importante pour la compatibilité et le tri. La commande `tr` est l'un des outils Unix classiques pour transformer les données textuelles. Fish Shell inclut aussi la fonction `string`, introduite dans les versions plus récentes, pour une manipulation de chaîne plus directe et lisible. Elle évite d'appeler des outils externes et améliore souvent la performance. Sous le capot, ces opérations tiennent compte du locale courant, ce qui affecte comment les transformations de casse sont appliquées en fonction de la langue.

## See Also (Voir Aussi)
- Documentation Fish pour les commandes `string`: https://fishshell.com/docs/current/cmds/string.html
- Manuel GNU pour `tr`: https://www.gnu.org/software/coreutils/manual/html_node/tr-invocation.html
