---
title:                "Écrire sur l'erreur standard"
date:                  2024-02-03T19:32:16.458369-07:00
model:                 gpt-4-0125-preview
simple_title:         "Écrire sur l'erreur standard"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/bash/writing-to-standard-error.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Quoi & Pourquoi ?
L'écriture sur l'erreur standard (stderr) en Bash consiste à diriger les messages d'erreur ou toute sortie de diagnostic importante séparée de la sortie standard (stdout). Les programmeurs font cela pour s'assurer que les messages d'erreur peuvent être facilement identifiés, enregistrés, ou même ignorés, aidant ainsi dans les processus de débogage et de journalisation.

## Comment faire :
En Bash, vous utilisez `>&2` pour rediriger la sortie vers stderr. Voici un exemple de base :

```bash
echo "Ceci est un message normal"
echo "Ceci est un message d'erreur" >&2
```

L'exécution de ce script affichera les deux messages dans la console, mais si vous les redirigez, vous pouvez séparer le stdout du stderr. Par exemple :

```bash
bash script.sh > output.txt 2> error.txt
```

`output.txt` contiendra `"Ceci est un message normal"`, tandis que `error.txt` capturera `"Ceci est un message d'erreur"`.

Pour un cas d'utilisation pratique, considérez un script qui traite des fichiers et signale une erreur si un fichier n'existe pas :

```bash
filename="exemple.txt"

if [ ! -f "$filename" ]; then
    echo "$filename n'existe pas !" >&2
    exit 1
else
    echo "Traitement de $filename"
fi
```

Exemple de sortie directement dans la console lorsque `exemple.txt` n'existe pas :

```
exemple.txt n'existe pas !
```

Il n'existe pas de bibliothèques tierces directes en Bash pour gérer stderr, car la redirection est nativement prise en charge et généralement suffisante. Cependant, pour des applications complexes, des cadres de journalisation ou des outils de journalisation externes comme `syslog` ou `log4bash` peuvent être incorporés pour gérer aussi bien stdout que stderr de manière plus efficace.
