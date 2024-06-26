---
date: 2024-01-20 17:57:17.349896-07:00
description: 'How to: (Comment faire :) Exemple simple avec `sed` .'
lastmod: '2024-04-05T21:53:59.438158-06:00'
model: gpt-4-1106-preview
summary: (Comment faire :) Exemple simple avec `sed` .
title: Recherche et remplacement de texte
weight: 10
---

## How to: (Comment faire :)
Exemple simple avec `sed` :
```Bash
echo "Bonjour le monde" | sed 's/monde/monde programmable/'
```
Sortie :
```
Bonjour le monde programmable
```

Remplacement dans un fichier :
```Bash
sed -i 's/ancien/nouveau/g' mon_fichier.txt
```

Utilisation des expressions régulières :
```Bash
grep '^[0-9]' fichier.txt | sed 's/^/Numéro: /'
```
Sortie pour chaque ligne commençant par un chiffre :
```
Numéro: [ligne commençante avec un chiffre]
```

Recherche récursive dans des fichiers avec `grep` :
```Bash
grep -R 'recherche' /chemin/du/dossier
```

## Deep Dive (Plongée en profondeur)
Rechercher et remplacer du texte est crucial en programmation depuis l'apparition des éditeurs de texte. `sed`, abréviation de "stream editor", est un outil classique de la ligne de commande UNIX, disponible depuis les années 70. Alternativement, des outils comme `awk`, `perl`, et des éditeurs de texte tels que `vim` ou `emacs` offrent également ces fonctionnalités, souvent avec des capacités plus avancées grâce aux expressions régulières. Concernant l'implémentation, `sed` lit le texte en entrée ligne par ligne, applique les transformations spécifiées, et affiche le résultat sur la sortie standard.

## See Also (Voir également)
- GNU sed manual : https://www.gnu.org/software/sed/manual/sed.html
- Introduction aux expressions régulières : https://www.regular-expressions.info/tutorial.html
- Tutoriel `grep` : https://www.tutorialspoint.com/unix_commands/grep.htm
