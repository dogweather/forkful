---
title:                "Vérifier l'existence d'un répertoire"
html_title:           "Bash: Vérifier l'existence d'un répertoire"
simple_title:         "Vérifier l'existence d'un répertoire"
programming_language: "Bash"
category:             "Bash"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/bash/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Pourquoi

Il est souvent utile de vérifier si un dossier existe avant de continuer avec un script Bash. Cela permet de s'assurer que toutes les opérations nécessaires peuvent être effectuées sans causer d'erreurs.

## Comment Faire

Il existe plusieurs façons de vérifier si un dossier existe en utilisant Bash. La méthode la plus simple consiste à utiliser l'opérateur de condition `-d`, qui vérifie s'il s'agit d'un dossier existant.

```
#!/bin/bash

if [ -d "/chemin/vers/dossier" ]; then
  echo "Le dossier existe."
else
  echo "Le dossier n'existe pas."
fi
```

Dans cet exemple, si le dossier existe, le script affichera "Le dossier existe." Sinon, il affichera "Le dossier n'existe pas.".

Une autre méthode consiste à utiliser la commande `test`. Cette commande peut vérifier différentes conditions, y compris si un fichier ou un dossier existe.

```
#!/bin/bash

if test -d "/chemin/vers/dossier"
then
  echo "Le dossier existe."
else
  echo "Le dossier n'existe pas."
fi
```

Vous pouvez également utiliser la commande `ls` avec l'option `-d` pour vérifier si un dossier spécifique existe dans un répertoire.

```
#!/bin/bash

# Vérifie si le dossier "dossier1" existe dans le répertoire "chemin/vers"
if ls -d /chemin/vers/dossier1; then
  echo "Le dossier existe."
else
  echo "Le dossier n'existe pas."
fi
```

## Approfondissement

Si vous souhaitez vérifier l'existence d'un dossier dans un script Bash et continuer avec des opérations ultérieures, vous pouvez utiliser une structure `if-else` comme dans les exemples ci-dessus. Cependant, si vous souhaitez simplement afficher un message d'erreur et quitter le script si le dossier n'existe pas, vous pouvez utiliser l'option `-e` avec `exit` dans la structure `if`.

```
#!/bin/bash

if [ -d "/chemin/vers/dossier" ]; then
  echo "Le dossier existe."
else
  echo "Le dossier n'existe pas."
  exit -1
fi
```

De plus, si vous souhaitez vérifier l'existence de plusieurs dossiers dans un script, vous pouvez utiliser une boucle `for` pour parcourir une liste de chemins de dossiers et vérifier chacun d'eux.

## Voir Aussi

- La documentation officielle de Bash sur les opérateurs de conditions : https://www.gnu.org/software/bash/manual/html_node/Bash-Conditional-Expressions.html
- Un autre article sur les meilleures pratiques pour vérifier si un dossier existe en Bash : https://linuxize.com/post/bash-check-if-directory-exists/