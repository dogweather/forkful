---
title:                "Extraction de sous-chaînes"
aliases: - /fr/bash/extracting-substrings.md
date:                  2024-01-20T17:45:05.679203-07:00
model:                 gpt-4-1106-preview
simple_title:         "Extraction de sous-chaînes"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/bash/extracting-substrings.md"
---

{{< edit_this_page >}}

## What & Why? (Quoi et Pourquoi ?)
Extraire des sous-chaînes, c'est récupérer des morceaux d'une chaîne de caractères. Pourquoi ? Pour analyser ou transformer des données, automatiser des tâches, ou tout simplement parce que vous avez besoin d'une partie spécifique d'une info.

## How to: (Comment faire :) 
Extraire avec des indices et des longueurs:

```Bash
# Extraire depuis la position 4, longueur 5
chaine="Je suis un script Bash épatant!"
sous_chaine="${chaine:4:5}"
echo $sous_chaine # sortie: suis
```
Utiliser l'expression régulière avec `grep`:

```Bash
echo "Facture: 12345" | grep -o -E '[0-9]+'
# sortie: 12345
```
## Deep Dive (Plongée Profonde)
Historiquement, l'extraction de sous-chaînes est essentielle pour transformer et transférer les données depuis l'ère du télétype. Bash permet cela avec la syntaxe `${chaine:index:longueur}`. C'est une approche simple qui marche bien pour des scripts courts et des tâches rapides. Vous pourriez aussi envisager des outils comme `awk`, `sed`, ou des langages de programmation avec des fonctions de manipulation de chaînes plus élaborées pour des besoins complexes.

## See Also (Voir aussi)
- GNU Bash documentation: https://www.gnu.org/software/bash/manual/
- Regular Expressions Info: https://www.regular-expressions.info/
- `awk` Tutorial: https://www.gnu.org/software/gawk/manual/gawk.html
- `sed` Stream Editor: https://www.gnu.org/software/sed/manual/sed.html
