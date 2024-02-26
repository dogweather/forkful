---
date: 2024-01-20 17:53:39.725483-07:00
description: "Lire un fichier texte, c'est parcourir et manipuler le contenu stock\xE9\
  . Les programmeurs le font pour traiter des donn\xE9es, configurer des logiciels,\
  \ et\u2026"
lastmod: '2024-02-25T18:49:54.703214-07:00'
model: gpt-4-1106-preview
summary: "Lire un fichier texte, c'est parcourir et manipuler le contenu stock\xE9\
  . Les programmeurs le font pour traiter des donn\xE9es, configurer des logiciels,\
  \ et\u2026"
title: Lecture d'un fichier texte
---

{{< edit_this_page >}}

## What & Why?
Lire un fichier texte, c'est parcourir et manipuler le contenu stocké. Les programmeurs le font pour traiter des données, configurer des logiciels, et souvent pour scripter des tâches automatisées.

## How to:
Voici différentes manières de lire un fichier en Bash:

```Bash
# Utiliser 'cat' pour afficher le contenu d'un fichier
cat monfichier.txt

# Lire ligne par ligne avec une boucle 'while' et 'read'
while IFS= read -r line; do
    echo "Ligne : $line"
done < monfichier.txt
```

Sample output pour la boucle `while`:
```Bash
Ligne : Première ligne de mon fichier
Ligne : Deuxième ligne
Ligne : Etc...
```

## Deep Dive
Historiquement, `cat` (concatenate) était souvent utilisé pour afficher le contenu d'un fichier sur l'écran. Mais en Bash, la boucle `while read` offre plus de contrôle et est plus adaptée pour traiter des fichiers ligne par ligne, surtout avec des fichiers volumineux. Alternativement, `awk` et `sed` sont puissants pour la manipulation de texte, mais sont plus complexes. 

La commande `cat` est simple et directe, mais si votre fichier est trop grand, vous gaspillez de la mémoire et du temps. "while IFS= read -r line" est lisible, sécuritaire (avec `-r` pas d'interprétation des backslashes) et facile à manipuler ligne par ligne. 

IFS (Internal Field Separator) est utilisé ici pour préserver les espaces en début de ligne, chose que `cat` fera automatiquement, mais pas `read`. C'est fondamental pour la fiabilité du script.

## See Also
- GNU Bash documentation: https://www.gnu.org/software/bash/manual/bash.html
- Advanced Bash-Scripting Guide: https://www.tldp.org/LDP/abs/html/
- 'BashGuide' de Greg's Wiki pour des pratiques sûres: http://mywiki.wooledge.org/BashGuide
