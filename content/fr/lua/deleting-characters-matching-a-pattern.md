---
title:                "Suppression de caractères correspondant à un motif"
html_title:           "C: Suppression de caractères correspondant à un motif"
simple_title:         "Suppression de caractères correspondant à un motif"
programming_language: "Lua"
category:             "Lua"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/lua/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Quoi & Pourquoi ?

Supprimer des caractères correspondant à un motif est un moyen efficace de nettoyer ou de manipuler des données en programmation. C'est parfait pour simplifier les chaînes de caractères, filtrer des informations inutiles, etc.

## Comment faire :

Voici comment vous pouvez supprimer des caractères correspondant à un motif en Lua :

```Lua
texte = "Bonjour les programmeurs !"
texte = texte:gsub("%s", "") -- supprime les espaces
print(texte) 
```

Dans cet exemple, `%s` est le motif qui correspond à tous les espaces. La fonction `gsub` remplace tous les caractères correspondant à ce motif par rien, supprimant ainsi tous les espaces.

Ce code affiche:

```Lua
Bonjourlesprogrammeurs!
```

## Exploration profonde

Historiquement, la possibilité de supprimer des caractères correspondant à un motif est apparue avec les premiers langages de programmation. Dans le contexte de Lua (inventé en 1993), les expressions régulières nous facilitent la tâche pour cette tâche.

Vous pouvez également utiliser la fonction `string.gsub` avec d'autres motifs pour supprimer différents types de caractères. Par exemple, `%w` correspond à tous les caractères alphanumériques, `%p` correspond à tous les signes de ponctuation, etc.

Il est également possible d'implémenter ce type de fonctionnalité récursive, bien qu'en Lua, utiliser `gsub` est généralement plus rapide et plus facile à lire.

## Voir aussi

Pour en savoir plus sur la manipulation de chaînes en Lua, vous pouvez consulter les liens suivants :

- [Programmation en Lua](https://www.lua.org/manual/5.3/manual.html#6.4)
- [Expressions Régulières en Lua](https://www.lua.org/pil/20.2.html)
- [Guide d'AllPatterns Lua](https://www.lua.org/pil/20.1.html)