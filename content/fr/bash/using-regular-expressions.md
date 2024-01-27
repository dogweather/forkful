---
title:                "Utilisation des expressions régulières"
date:                  2024-01-19
html_title:           "Bash: Utilisation des expressions régulières"
simple_title:         "Utilisation des expressions régulières"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/bash/using-regular-expressions.md"
---

{{< edit_this_page >}}

## What & Why? (Quoi et Pourquoi ?)
Les expressions régulières (regex) sont des motifs de chaînes utilisés pour rechercher et manipuler du texte. Les développeurs les utilisent pour la simplicité et l'efficacité qu'elles apportent dans le filtrage et le traitement des données textuelles.

## How to: (Comment faire :)
```Bash
# Recherche d'un motif simple
echo "Le renard brun rapide saute par-dessus le chien paresseux" | grep 'renard'

# Utilisation des classes de caractères
echo "Bonjour 123" | grep '[0-9]'

# Capturer un groupe et le réutiliser
echo "File_1234.png" | sed -r 's/(File_)([0-9]+)\.png/\1\2_modified.png/'

# Exemple avec grep et regex pour filtrer des adresses email
cat emails.txt | grep -E '[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\.[a-zA-Z]{2,4}'
```
Output:
```
Le renard brun rapide saute par-dessus le chien paresseux
123
File_1234_modified.png
# Les adresses email correspondantes dans `emails.txt`
```

## Deep Dive (Plongée en profondeur)
Les regex sont nées dans les années 1950 avec les travaux du mathématicien Stephen Kleene. Alternatives: awk, perl. En bash, `grep` et `sed` utilisent les regex POSIX par défaut; pour plus de fonctionnalités, activez les expressions étendues avec `-E`.

## See Also (Voir aussi)
- La page de manuel de `grep` via la commande `man grep` dans le terminal.
- [Regular-Expressions.info](https://www.regular-expressions.info/)
- [GNU Sed](https://www.gnu.org/software/sed/manual/sed.html), pour une exploration plus approfondie de `sed`.
- [The Open Group Base Specifications Issue 7, 2018 edition - Shell & Utilities: grep](https://pubs.opengroup.org/onlinepubs/9699919799/utilities/grep.html), standards techniques pour `grep`.
- [AWK - A Pattern Scanning and Processing Language](https://www.gnu.org/software/gawk/manual/gawk.html), documentation de AWK, une alternative à regex en bash.
