---
title:                "Création d'un fichier temporaire"
html_title:           "Fish Shell: Création d'un fichier temporaire"
simple_title:         "Création d'un fichier temporaire"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/fish-shell/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

Qu'est-ce que créer un fichier temporaire et pourquoi les programmeurs le font-ils?

Créer un fichier temporaire, c'est créer un fichier éphémère qui va être utilisé pour stocker des données temporaires. Les programmeurs le font souvent lorsqu'ils ont besoin de générer ou de manipuler des données temporaires sans affecter leurs fichiers existants.

Comment faire:

```
Fish Shell vous permet de créer facilement des fichiers temporaires en utilisant la commande `mktemp`. Voici un exemple de code :

```fish
# Créer un fichier temporaire nommé "mon_fichier_temp"
set mon_fichier_temp (mktemp)

# Écrire du contenu dans le fichier temporaire
echo "Données temporaires" > $mon_fichier_temp

# Lire le contenu du fichier temporaire
cat $mon_fichier_temp
```

Output: Données temporaires

Deep Dive:

Avant l'avènement de Fish Shell, les programmeurs utilisaient souvent la commande `touch` pour créer des fichiers temporaires vides. Cependant, cette méthode pouvait entraîner la création accidentelle de fichiers avec le même nom que des fichiers existants.

Une alternative à la commande `mktemp` est la commande `mkfile`, disponible sur les systèmes d'exploitation macOS. Cependant, celle-ci n'offre pas autant de flexibilité, car elle crée uniquement des fichiers de taille fixe.

Pour ceux qui veulent en savoir plus, la création d'un fichier temporaire passe par la génération d'un nom unique à l'aide d'un processus aléatoire. Ce nom est ensuite utilisé pour créer le fichier temporaire à l'aide de la commande `touch`.

Voir aussi:

Pour en savoir plus sur Fish Shell, vous pouvez consulter sa documentation officielle (https://fishshell.com/docs/current/). Vous pouvez également trouver d'autres articles et tutoriels en ligne pour apprendre à utiliser les fonctionnalités avancées de Fish Shell.