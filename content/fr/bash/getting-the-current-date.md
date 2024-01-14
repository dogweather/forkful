---
title:    "Bash: Obtenir la date actuelle"
keywords: ["Bash"]
---

{{< edit_this_page >}}

## Pourquoi

Avez-vous déjà été en train de travailler sur un projet et avez eu besoin de connaître la date actuelle ? Ou peut-être avez-vous simplement besoin de connaître la date pour des raisons personnelles. Dans les deux cas, il peut être utile d'avoir un moyen simple pour obtenir la date actuelle dans un script Bash. Dans cet article, nous allons vous montrer comment obtenir la date actuelle en utilisant Bash.

## Comment faire

Il y a plusieurs façons d'obtenir la date actuelle en utilisant Bash. La première méthode consiste à utiliser la commande `date` avec différents formats de date. Par exemple, en utilisant la commande suivante :

```Bash
date +%d-%m-%Y
```

La sortie serait la date actuelle au format date/mois/année. Vous pouvez également utiliser la commande `date` avec d'autres options de format pour obtenir uniquement certaines parties de la date, telles que l'heure ou le jour de la semaine.

Une autre méthode consiste à utiliser la variable d'environnement `$DATE`. Cette variable contient la date actuelle sous forme de nombre. Vous pouvez alors utiliser la commande `date` pour formater ce nombre en une date lisible, comme ceci :

```Bash
echo $DATE | date -f
```

Enfin, vous pouvez également utiliser la commande `cal` pour afficher le calendrier du mois en cours, avec la date actuelle marquée en surbrillance. Vous pouvez utiliser cette méthode si vous souhaitez une vue d'ensemble de la date actuelle par rapport au reste du mois.

## Plongée en profondeur

Maintenant que vous savez comment obtenir la date actuelle en Bash, voyons quelques détails supplémentaires. Le format de date que nous avons utilisé jusqu'à présent, `%d-%m-%Y`, suit la norme ISO-8601. Cela signifie que le jour vient en premier, suivi du mois et de l'année. Vous pouvez utiliser différents formats selon vos préférences en utilisant la commande `date`.

Il est également possible de définir le fuseau horaire dans la commande `date`, afin d'obtenir la date et l'heure pour un endroit spécifique. Vous pouvez utiliser la variable d'environnement `$TZ` pour spécifier le fuseau horaire, ou utiliser des abréviations telles que "PST" ou "EST".

Enfin, il est important de noter que la commande `date` utilise le fuseau horaire du système par défaut. Si vous avez besoin de récupérer la date et l'heure dans un fuseau horaire spécifique sans modifier celui du système, vous devrez utiliser des outils externes, tels que `TZ` ou `dateutil` en Python.

## Voir aussi

Maintenant que vous savez comment obtenir la date actuelle en utilisant Bash, vous pouvez également explorer d'autres fonctionnalités utiles de ce langage de script. Vous pouvez également consulter ces liens pour en savoir plus sur l'utilisation des dates en Bash :

- [Documentation officielle de Bash](https://www.gnu.org/software/bash/manual/html_node/Dates-and-times.html)
- [Guide de référence rapide pour la manipulation de dates en Bash](https://blog.balthazar-rouberol.com/manipulating-dates-in-bash)
- [Tutoriel sur les fonctions de dates en Bash](https://www.tecmint.com/generate-dates-on-linux-using-bash/)

Merci d'avoir lu cet article sur la façon d'obtenir la date actuelle en utilisant Bash. Nous espérons que cela vous a été utile dans vos projets et votre utilisation quotidienne. N'hésitez pas à explorer et à pratiquer ces techniques pour en savoir plus sur les fonctionnalités de Bash.