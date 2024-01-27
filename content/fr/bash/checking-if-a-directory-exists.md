---
title:                "Vérification de l'existence d'un répertoire"
date:                  2024-01-19
html_title:           "Bash: Vérification de l'existence d'un répertoire"
simple_title:         "Vérification de l'existence d'un répertoire"
programming_language: "Bash"
category:             "Bash"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/bash/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## What & Why?
Vérifier l'existence d'un répertoire confirme que le dossier que vous souhaitez utiliser est bien là où vous le pensez. Les programmeurs font ça pour éviter des erreurs chiantes: imaginez écrire dans un dossier qui n'existe pas!

## How to:
Pour voir si un répertoire existe, utilisez `if` avec `[-d /chemin/du/repertoire]`. Voici la syntaxe et un exemple :

```Bash
if [ -d /chemin/du/repertoire ]; then
  echo "Le répertoire existe!"
else
  echo "Le répertoire n'existe pas."
fi
```

Si `/chemin/du/repertoire` existe, vous verrez :

```
Le répertoire existe!
```
Sinon :

```
Le répertoire n'existe pas.
```

## Deep Dive
Historiquement sous Unix, la vérification de l'existence d'un fichier ou d'un répertoire est fondamentale. Elle assure que le script ne plante pas pour une faute aussi bête qu'un mauvais chemin. En alternative au test `[ -d ]`, il y a `[[ -d /chemin/du/repertoire ]]` pour des scripts plus modernes et `if [ "$(ls -A /chemin/du/repertoire)" ]` si vous voulez aussi savoir si le répertoire est vide. Néanmoins, la première option reste la plus directe et efficace.

## See Also
- Le manuel de Bash : `man bash`
- Guide avancé de script Bash : https://www.tldp.org/LDP/abs/html/
- Comparaison des tests de fichiers dans Bash : https://mywiki.wooledge.org/BashGuide/TestsAndConditionals
