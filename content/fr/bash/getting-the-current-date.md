---
title:                "Obtenir la date actuelle"
html_title:           "Bash: Obtenir la date actuelle"
simple_title:         "Obtenir la date actuelle"
programming_language: "Bash"
category:             "Bash"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/bash/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Qu'est-ce que c'est et pourquoi le faisons-nous? 

Obtenir la date actuelle est une tâche courante en programmation, cela permet de garder une trace du moment exact où un programme s'exécute ou d'afficher la date à l'utilisateur. C'est une fonctionnalité simple mais utile dans de nombreux cas. 

## Comment faire: 

Vous pouvez facilement obtenir la date actuelle en utilisant la commande `date` dans un script Bash. Voici un exemple de code et sa sortie:

```Bash
#! /bin/bash
echo "La date actuelle est: $(date)"
```
Sortie: `La date actuelle est: Tue Apr 20 11:00:00 UTC 2021`

## Plongée en profondeur: 

La commande `date` est disponible sur de nombreux systèmes Unix et Linux depuis les années 1970. Elle peut non seulement afficher la date actuelle, mais aussi la modifier et afficher d'autres informations sur le temps et la date. Si vous recherchez une alternative à la commande `date`, il existe d'autres commandes telles que `cal` pour afficher un calendrier et `clock` pour afficher l'heure actuelle.

L'implémentation de la commande `date` peut varier légèrement selon le système d'exploitation, mais elle repose généralement sur la fonction `time()` du langage C pour obtenir l'heure actuelle à partir de l'horloge système. Il existe également des bibliothèques de programmation pour gérer les dates et les heures de manière plus complexe et précise.

## Voir aussi: 

Pour en savoir plus sur la commande `date` et ses fonctionnalités, vous pouvez consulter la documentation officielle en tapant `man date` dans votre terminal. Vous y trouverez toutes les options disponibles et des exemples d'utilisation. Vous pouvez également explorer d'autres commandes de manipulation de temps et de dates en Bash pour enrichir vos connaissances en programmation.