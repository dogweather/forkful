---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:32:16.458369-07:00
description: "L'\xE9criture sur l'erreur standard (stderr) en Bash consiste \xE0 diriger\
  \ les messages d'erreur ou toute sortie de diagnostic importante s\xE9par\xE9e de\
  \ la sortie\u2026"
lastmod: '2024-03-13T22:44:58.014926-06:00'
model: gpt-4-0125-preview
summary: "L'\xE9criture sur l'erreur standard (stderr) en Bash consiste \xE0 diriger\
  \ les messages d'erreur ou toute sortie de diagnostic importante s\xE9par\xE9e de\
  \ la sortie\u2026"
title: "\xC9crire sur l'erreur standard"
weight: 25
---

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
