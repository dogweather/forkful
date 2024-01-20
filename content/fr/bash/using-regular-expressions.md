---
title:                "Utiliser les expressions régulières"
html_title:           "C: Utiliser les expressions régulières"
simple_title:         "Utiliser les expressions régulières"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/bash/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Qu'est-ce que c'est & Pourquoi ?

Les expressions régulières, aussi appelées "regex", sont des suites de caractères formant un motif de recherche. Elles sont utilisées pour manipuler des chaînes de caractères (le texte) en facilitant les tâches de recherche, d'extraction et de remplacement d'information. 

## Comment faire:

Voici des exemples simples pour démarrer.

Pour trouver tous les fichiers dans un répertoire qui ont 'txt' dans leur nom:

```Bash
ls | grep 'txt'
```

Pour trouver tous les chiffres dans une chaîne:

```Bash
echo "abc123xyz" | grep -o '[0-9]'
```

Affiche:

```Bash
1
2
3
```

## Approfondissement

Historiquement, les regex ont été introduites dans les années 50 avec l'éditeur de texte QED. 

Comme alternatives, certaines langages (comme Python) ont leurs propres bibliothèques pour manipuler des chaînes sans avoir besoin des regex, et les SGBD ont des fonctionnalités pour gérer directement les chaînes au niveau de la base de données.

Les expressions régulières sont implémentées différemment selon les langages de programmation. En Bash, ils sont supportés depuis la version 3.0 grâce à l'opérateur `=~`. 

## Voir aussi

- Tutorial Bash : https://tldp.org/HOWTO/Bash-Prog-Intro-HOWTO.html
- Guide des expressions régulières : https://www.regular-expressions.info/tutorial.html
- Documentation officielle de la commande `grep` : http://man7.org/linux/man-pages/man1/grep.1.html