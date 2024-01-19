---
title:                "Lecture d'un fichier texte"
html_title:           "Arduino: Lecture d'un fichier texte"
simple_title:         "Lecture d'un fichier texte"
programming_language: "Bash"
category:             "Bash"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/bash/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Pourquoi et Pour Quoi?
Lire un fichier texte en Bash signifie récupérer ses données, ligne par ligne, du début à la fin. Les programmeurs utilisent cela pour manipuler une variété de tâches automatisées, comme l'analyse de logs, générer des rapports, ou même programmer des scripts de maintenance.

## Comment Faire :
Lisons un fichier texte simple en Bash. Mettons que vous avez le fichier `fichier.txt` qui contient les lignes :

```Bash
Bonjour le monde
Je suis un développeur
J'aime bash
```

Une façon commune de lire ce fichier est :

```Bash
while IFS= read -r line
do
    echo "$line"
done < "fichier.txt"
```

Ce qui vous donnera en sortie :

```Bash
Bonjour le monde
Je suis un développeur
J'aime bash
```

## Un Peu Plus Profond :
Historiquement, Bash (Bourne Again SHell) a été créé pour remplacer le Shell Bourne dans les systèmes Unix. Lire un fichier en Bash est une fonction présente depuis les premières versions.

Il y a des alternatives pour lire un fichier. Par exemple, la commande `cat` peut être utilisée pour afficher le contenu d'un fichier directement, sans boucle.

```Bash
cat fichier.txt
```

Notez que cette méthode n'est pas recommandée si vous voulez manipuler le contenu ligne par ligne.

Le `read` dans la boucle `while` récupère tour à tour chaque ligne du fichier. L'option `-r` empêche le traitement des backslashes (\) comme des caractères d'échappement.

## Voir Aussi :
Pour plus d`'informations sur la programmation bash, consultez ces liens :
- GNU Bash : https://www.gnu.org/software/bash/ 
- Référence Bash: https://www.gnu.org/software/bash/manual/bash.html
- Guide avancé de Bash-Scripting : http://tldp.org/LDP/abs/html/