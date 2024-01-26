---
title:                "Mettre une chaîne de caractères en majuscules"
html_title:           "C: Mettre une chaîne de caractères en majuscules"
simple_title:         "Mettre une chaîne de caractères en majuscules"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/bash/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## What & Why?
Capitaliser une chaîne, c'est transformer toutes ses lettres en majuscules. C'est pratique pour normaliser des données textuelles et pour mettre en avant des éléments importants.

## How to:
Bash utilise `tr` pour transformer les lettres en majuscules :

```Bash
$ chaine="bonjour, le monde!"
$ echo $chaine | tr '[:lower:]' '[:upper:]'
BONJOUR, LE MONDE!
```

Plus court avec une variable en Bash 4+ :

```Bash
$ chaine="bonjour, le monde!"
$ echo "${chaine^^}"
BONJOUR, LE MONDE!
```

## Deep Dive
Historique : `tr` (translate) existe depuis les premières versions d'UNIX, utilisé pour substituer ou supprimer des caractères. 

Alternatives : `awk`, `sed`, ou des langages de script comme Python offrent aussi des moyens de capitaliser des chaînes.

Détails : Dans Bash 4+, `^^` rend toute la chaîne capitale, alors que `^` capitalise uniquement le premier caractère. Pour les versions antérieures, `tr` est souvent la meilleure solution intégrée.

## See Also
- La page manuel de `tr`: [`man tr`](https://linux.die.net/man/1/tr)
- Guide de Bash : [Advanced Bash-Scripting Guide](https://www.tldp.org/LDP/abs/html/)
- Développer vos connaissances sur les expressions régulières : [Regular-Expressions.info](https://www.regular-expressions.info/)
