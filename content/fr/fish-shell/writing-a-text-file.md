---
title:                "Rédiger un fichier texte"
aliases:
- fr/fish-shell/writing-a-text-file.md
date:                  2024-02-03T19:27:36.462027-07:00
model:                 gpt-4-0125-preview
simple_title:         "Rédiger un fichier texte"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/fish-shell/writing-a-text-file.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Quoi & Pourquoi ?

Écrire dans un fichier texte avec Fish Shell vous permet de stocker des données de manière persistante, facilitant ainsi la récupération ou la manipulation des données soit par le même script Fish soit par d'autres programmes. Les programmeurs font cela pour enregistrer des logs, sauvegarder des configurations ou exporter des données pour un traitement ultérieur.

## Comment faire :

Pour écrire dans un fichier texte en Fish, vous pouvez utiliser la commande `echo` combinée avec des opérateurs de redirection. Il n'y a pas de bibliothèques tierces populaires spécifiquement pour l'écriture de fichiers en Fish, car les commandes intégrées du shell sont simples et efficaces à cet effet.

### Écrire du texte dans un nouveau fichier ou écraser un fichier existant :
```fish
echo "Bonjour, Fish Shell !" > output.txt
```
Cette commande écrit "Bonjour, Fish Shell !" dans `output.txt`, en créant le fichier s'il n'existe pas ou en l'écrasant s'il existe.

### Ajouter du texte à un fichier existant :
Si vous souhaitez ajouter du texte à la fin d'un fichier existant sans supprimer son contenu actuel, utilisez l'opérateur d'ajout `>>` :
```fish
echo "Ajout d'une nouvelle ligne au fichier." >> output.txt
```

### Écrire plusieurs lignes :
Vous pouvez écrire plusieurs lignes dans un fichier en utilisant echo avec un caractère de nouvelle ligne `\n`, ou vous pouvez enchaîner plusieurs commandes echo à l'aide de points-virgules :
```fish
echo "Première Ligne\nDeuxième Ligne" > output.txt
# OU
echo "Première Ligne" > output.txt; echo "Deuxième Ligne" >> output.txt
```

### Exemple de sortie :
Pour voir le contenu de `output.txt` après avoir exécuté les commandes ci-dessus, utilisez la commande `cat` :
```fish
cat output.txt
```
```plaintext
Première Ligne
Deuxième Ligne
```
Remplacer ou ajouter des textes comme montré manipule le contenu du fichier selon vos besoins, démontrant des manières simples mais puissantes de travailler avec des fichiers texte en Fish Shell.
