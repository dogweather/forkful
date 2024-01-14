---
title:                "Bash: Calculer une date dans le futur ou le passé"
simple_title:         "Calculer une date dans le futur ou le passé"
programming_language: "Bash"
category:             "Bash"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/bash/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

# Pourquoi

La capacité de calculer une date dans le futur ou dans le passé peut être incroyablement utile dans la programmation Bash. Cela peut vous permettre de planifier des tâches, de générer des rapports basés sur des données temporelles ou simplement de vous faciliter la vie lors de l'utilisation de la ligne de commande. Dans cet article, nous allons vous montrer comment réaliser cela en utilisant des exemples de code Bash et en vous plongeant plus en profondeur dans les détails de ce processus.

## Comment faire

La façon la plus courante de calculer une date dans le futur ou dans le passé en Bash est d'utiliser la commande `date`. Cette commande peut être utilisée avec différents arguments pour spécifier la date de départ, la quantité de temps à ajouter ou soustraire, et le format de date souhaité pour le résultat.

Voici un exemple de code qui utilise la commande `date` pour calculer la date dans 10 jours à partir d'aujourd'hui et afficher le résultat dans le format jour/mois/année :

```Bash
future_date=$(date +%d/%m/%Y -d "+10 days")
echo $future_date # affiche 13/04/2021 si on est le 3/4/2021
```

Nous pouvons également utiliser des arguments tels que `weeks`, `months` ou `years` dans la commande `date` pour spécifier une période de temps plus longue. Voici un autre exemple qui calcule la date dans 4 semaines et l'affiche dans le format année-mois-jour :

```Bash
future_date=$(date +%Y-%m-%d -d "4 weeks")
echo $future_date # affiche la date actuelle dans 4 semaines
```

## Plongée Profonde

La commande `date` peut sembler simple, mais sa flexibilité rend possible un large éventail de calculs de dates dans le futur ou dans le passé. Par exemple, nous pouvons également utiliser des arguments tels que `first monday' ou `last friday` pour spécifier une date précise en utilisant des jours de la semaine.

Nous pouvons également combiner la commande `date` avec d'autres commandes Bash, telles que `awk` ou `sed`, pour effectuer des calculs basés sur des données provenant d'autres fichiers ou commandes.

Si vous souhaitez en savoir plus sur toutes les options disponibles pour la commande `date`, vous pouvez consulter sa documentation complète en tapant `man date` dans votre terminal.

## Voir aussi
- [Documentation officielle de la commande `date`](https://manpages.debian.org/stretch/coreutils/date.1.html)
- [Tutoriel Bash pour les débutants](https://www.freecodecamp.org/news/linux-bash-shell-programming-fundamentals-beginners-learn-basics-for-free-85063ddd0780/)
- [Cheat sheet Bash pour les opérations sur les dates](https://devhints.io/bash-date)