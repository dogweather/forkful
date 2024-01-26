---
title:                "Écriture d'un fichier texte"
html_title:           "Arduino: Écriture d'un fichier texte"
simple_title:         "Écriture d'un fichier texte"
programming_language: "Bash"
category:             "Bash"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/bash/writing-a-text-file.md"
---

{{< edit_this_page >}}

## What & Why?
Écrire un fichier texte permet de sauvegarder des données de façon persistante. Les programmeurs le font pour stocker des configurations, des logs, ou échanger des infos entre processus.

## How to:
Créez et écrivez dans un fichier avec `echo` ou `printf`. Utilisez `>` pour écraser ou `>>` pour ajouter à un fichier.

```Bash
echo "Salut le monde!" > hello.txt
cat hello.txt
```
Sortie:
```
Salut le monde!
```

Ajoutez du texte sans écraser:
```Bash
echo "À bientôt!" >> hello.txt
cat hello.txt
```
Sortie:
```
Salut le monde!
À bientôt!
```

## Deep Dive
Dans le passé, `>` et `>>` viennent du shell original de Unix. Pour de gros fichiers, `sed`, `awk` ou des langages de script comme Python peuvent être plus performants. Les détails impliquent la gestion des descripteurs de fichiers et l'écriture bufferisée.

## See Also
- Guide avancé de Bash: https://www.gnu.org/savannah-checkouts/gnu/bash/manual/bash.html
- Tutoriel sur les redirections: https://tldp.org/LDP/abs/html/io-redirection.html
