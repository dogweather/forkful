---
title:    "Bash: Obtenir la date actuelle"
keywords: ["Bash"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fr/bash/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Pourquoi: 

Obtenir la date actuelle peut sembler une tâche simple et banale, mais cela peut être très utile pour les programmeurs Bash. Que ce soit pour créer des fichiers avec une date en tant que nom ou pour exécuter des tâches en fonction de la date, connaître la date actuelle est un outil précieux pour tout programmeur.

## Comment faire: 

La bonne nouvelle est qu'il est très facile d'obtenir la date actuelle en utilisant Bash. Il suffit d'utiliser la commande `date`. Voici un exemple de code Bash pour obtenir la date actuelle : 

```Bash
current_date=$(date +"%Y-%m-%d")
echo "La date actuelle est : $current_date"
```

La sortie de ce code sera quelque chose comme ceci : `La date actuelle est : 2021-03-26`. Vous pouvez également personnaliser le format de la date en utilisant les options de la commande `date`. Par exemple, pour obtenir la date et l'heure actuelles, vous pouvez utiliser `date +"%Y-%m-%d %H:%M:%S"`.

Il est également possible d'afficher la date et l'heure d'un fuseau horaire spécifique en utilisant l'option `-u` suivi du fuseau horaire. Par exemple, `date -u +"%Y-%m-%d %H:%M:%S" UTC` affichera la date et l'heure actuelles en temps universel coordonné. 

## Plongée en profondeur: 

La commande `date` est en fait un programme écrit en C qui utilise la bibliothèque système pour obtenir la date et l'heure actuelles. Elle peut également être utilisée pour modifier la date et l'heure du système. En utilisant les options `+"%s"` et `+"%s.%N"`, on peut obtenir la date et l'heure sous forme de timestamp Unix en secondes ou en nanosecondes respectivement.

Pour en savoir plus sur les différentes options et fonctionnalités de la commande `date`, vous pouvez consulter sa page de manuel en tapant `man date` dans votre terminal.

## Voir aussi: 

- [Documentation sur la commande `date`](https://www.gnu.org/software/coreutils/manual/html_node/date-invocation.html)
- [Utilisation de la commande `date` pour modifier la date et l'heure système](https://www.tecmint.com/set-time-timezone-and-synchronize-time-using-timedatectl-command/)
- [Formatage de la date et de l'heure dans Bash](https://www.linuxjournal.com/content/validating-date-and-time-bash-script-using-date)

Merci d'avoir lu cet article sur la façon d'obtenir la date actuelle en utilisant Bash. Nous espérons que cela vous sera utile dans vos futurs projets de programmation. N'hésitez pas à explorer davantage la commande `date` et à découvrir toutes ses fonctionnalités. Bonne programmation !