---
title:                "Création d'un fichier temporaire"
html_title:           "Kotlin: Création d'un fichier temporaire"
simple_title:         "Création d'un fichier temporaire"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/fish-shell/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Qu'est-ce que & Pourquoi?

La création d'un fichier temporaire est un processus qui génère un emplacement de stockage à usage unique et à court terme pour les données. Les programmeurs le font pour manipuler des informations transitoires sans affecter les fichiers permanents.

## Comment faire:

Voici comment vous pourriez créer un fichier temporaire avec Fish Shell :

```Fish Shell
set temp_file (mktemp)
echo 'Bonjour le Monde!' > $temp_file
cat $temp_file
```

L'exécution de ce script affichera 'Bonjour le Monde!'. Remarquez à quel point cela est facile et simple!

## Plongée Profonde

Historiquement, l'utilisation des fichiers temporaires remonte aux premiers jours du calcul. Ils sont restés un outil précieux pour les programmeurs en raison de leur commodité et de leur flexibilité.

Il existe des alternatives à la création de fichiers temporaires, notamment l'utilisation de variables ou de mémoire tampon, bien que chacune ait ses propres avantages et inconvénients.

Pour générer un fichier temporaire dans Fish Shell, nous avons utilisé la commande 'mktemp', qui crée le fichier puis renvoie son nom. La variable `temp_file` est utilisée pour stocker ce nom afin que nous puissions interagir avec le fichier ultérieurement.

## Voir Aussi

Vous pouvez en savoir plus sur la programmation avec Fish Shell en consultant les liens suivants:
1. [Fish Shell Documentation Officielle](https://fishshell.com/docs/current/index.html)
2. [Introduction à Fish Shell](https://flaviocopes.com/fish-shell/)
3. [Fish Shell pour les Bashers](https://olivergondza.github.io/2019/01/20/fishshell.html)

N'oubliez pas, apprenez en faisant. Continuez à expérimenter!