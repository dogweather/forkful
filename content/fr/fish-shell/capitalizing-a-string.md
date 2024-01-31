---
title:                "Mettre une chaîne de caractères en majuscules"
date:                  2024-01-19
html_title:           "C: Mettre une chaîne de caractères en majuscules"
simple_title:         "Mettre une chaîne de caractères en majuscules"

category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/fish-shell/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## What & Why?
Capitaliser une chaîne, c'est transformer toutes les lettres en majuscules. Les programmeurs font ça pour normaliser les inputs utilisateur, pour des raisons esthétiques ou pour respecter certaines conventions.

## How to:
Avec Fish, capitaliser une chaîne c'est facile. Voici comment faire :

```Fish Shell
# Déclarer une variable avec du texte
set phrase "bonjour le monde"

# Capitaliser la chaîne et imprimer le résultat
echo $phrase | string to-upper
```

Sortie :

```
BONJOUR LE MONDE
```

## Deep Dive
Historiquement, la capitalisation dans les systèmes informatiques pouvait indiquer un statut important, comme dans le cas des noms de fichiers UNIX des années 70. En Fish, `string to-upper` est simple mais puissant. Alternativement, vous pouvez utiliser `awk`, `tr`, ou `sed` dans un script bash, mais Fish offre une syntaxe plus claire.

En interne, `string to-upper` utilise les fonctionnalités de conversion de caractères de Unicode, ce qui signifie qu'elle gère correctement les caractères non ASCII.

## See Also
Pour aller plus loin avec `string` dans Fish :

- Documentation officielle de `string`: https://fishshell.com/docs/current/cmds/string.html
- Unicode Standard pour la casse des lettres: https://www.unicode.org/faq/casemap_charprop.html
- Forum Fish, pour poser des questions ou partager des astuces: https://fishshell.com/docs/current/index.html#discussion
