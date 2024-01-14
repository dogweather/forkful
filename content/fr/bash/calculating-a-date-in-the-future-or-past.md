---
title:    "Bash: Calculer une date dans le futur ou le passé"
keywords: ["Bash"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fr/bash/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

# Pourquoi

Il est souvent nécessaire de calculer une date dans le futur ou dans le passé lors de la programmation en Bash. Que ce soit pour des tâches de planification ou de gestion de données, cette compétence est essentielle pour un programmeur.

# Comment faire

La première étape pour calculer une date dans le futur ou dans le passé est de déterminer la date de référence. Cela peut être la date actuelle, une date précédemment saisie ou une date fournie par l'utilisateur.

Ensuite, il faut décider du nombre d'années, de mois ou de jours à ajouter ou à soustraire à cette date de référence. Dans Bash, cela peut être fait en utilisant la commande "date -d" suivie de la durée souhaitée.

Par exemple, pour calculer la date dans 6 mois à partir de la date actuelle :

```Bash
date -d "6 months"
```

Le résultat sera affiché au format par défaut de la date et de l'heure.

Pour obtenir un format spécifique de sortie, il suffit d'ajouter la commande "date" à la fin avec les options de formatage souhaitées.

Par exemple, pour obtenir la date dans 6 mois au format AAAA-MM-JJ :

```Bash
date -d "6 months" +"%Y-%m-%d"
```

Cela peut également être utilisé pour soustraire des durées à la date de référence :

```Bash
date -d "1 year ago"
```

Même principe pour formater la sortie selon nos besoins :

```Bash
date -d "1 year ago" +"%d/%m/%Y"
```

# Plongée en profondeur

En plus des durées de temps simples telles que "6 months" ou "1 year ago", les commandes "date -d" permettent également de gérer des dates à l'aide de valeurs absolues.

Par exemple, pour calculer la date dans 10 jours à partir du 15 mars 2021 :

```Bash
date -d "2021/03/15 + 10 days"
```

De plus, il est possible de choisir une date de référence différente en utilisant le paramètre "-u" qui prend en compte le fuseau horaire UTC.

```Bash
date -d "-6 hours + 1 week" -u
```

Notez que l'ordre des valeurs est important, la commande utilisera la date de référence en premier avant de soustraire ou d'ajouter la durée spécifiée.

# Voir aussi

- [Documentation officielle de la commande "date"](https://www.gnu.org/software/coreutils/manual/html_node/Date-input-formats.html)
- [Tutoriel sur la manipulation de dates en Bash](https://openclassrooms.com/fr/courses/43538-reprenez-le-controle-a-laide-de-linux/3737021-manipulez-les-dates-et-les-heures)
- [Astuce pour ajouter une date à un fichier en Bash](https://stackoverflow.com/questions/17993449/appending-date-to-filename-in-bash)