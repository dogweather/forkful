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

## Pourquoi

👉 Obtenir la date et l'heure actuelles est souvent utile lorsque l'on écrit un script Bash. Que ce soit pour afficher la date dans un fichier de sortie, pour enregistrer la date d'exécution d'un script, ou pour toute autre utilisation, il est important de savoir comment obtenir ces informations à partir de la ligne de commande.

## Comment faire

Voici quelques exemples de code montrant comment obtenir la date et l'heure actuelles en utilisant Bash :

```bash
# Afficher la date et l'heure actuelles au format UTC
date

# Afficher la date et l'heure au format spécifié (ici, mois/jour/année)
date +%m/%d/%Y

# Stocker la date et l'heure dans une variable pour une utilisation ultérieure
now=$(date)

# Afficher uniquement l'heure actuelle
date +%H:%M
```

Output :

```
sam. août 21 15:31:22 UTC 2021
08/21/2021
15:31
```

## Approfondissement

La commande `date` est utilisée pour afficher ou modifier la date et l'heure système. Elle permet également de formater la sortie selon nos besoins en utilisant des indicateurs spécifiques comme vu dans les exemples précédents.

On peut également spécifier un fuseau horaire particulier en utilisant l'option `-d`, comme ceci :

```bash
date -d '3 hours ago'
```

Output :

```
sam. août 21 12:31:22 UTC 2021
```

De plus, la commande `date` peut également être utilisée pour définir la date et l'heure du système en utilisant l'option `-s`. Il est important de noter que cette commande nécessite des privilèges d'administrateur pour être exécutée.

Pour en savoir plus sur la commande `date`, vous pouvez consulter sa page de manuel en utilisant la commande `man date` ou en en savoir plus sur la [documentation officielle de Bash](https://www.gnu.org/software/bash/manual/html_node/Bash-Date-Manipulation.html).

## Voir aussi

📚 Si vous souhaitez en apprendre davantage sur la programmation en Bash, voici quelques ressources utiles :

- [Guide de démarrage rapide de Bash](https://linux.die.net/Bash-Quick-Start)
- [Cours interactif pour débuter avec Bash](https://learnxinyminutes.com/docs/bash/)
- [Tutoriels et exemples de scripts Bash](https://bash.cyberciti.biz/guide/Main_Page)
- [Documentation officielle de Bash](https://www.gnu.org/software/bash/manual/)

Maintenant que vous savez comment obtenir la date et l'heure actuelles en Bash, il ne vous reste plus qu'à expérimenter et à l'utiliser dans vos scripts ! 💻