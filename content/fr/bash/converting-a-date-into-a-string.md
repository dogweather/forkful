---
title:    "Bash: Transformation d'une date en chaîne de caractères"
keywords: ["Bash"]
---

{{< edit_this_page >}}

## Pourquoi

Convertisser une date en chaîne de caractères est une tâche courante en programmation Bash. Cela peut être utile pour afficher des dates dans un format spécifique ou pour les utiliser dans une opération de comparaison avec d'autres dates. Dans cet article, nous allons explorer comment convertir une date en chaîne de caractères en utilisant Bash.

## Comment faire

La première étape pour convertir une date en chaîne de caractères est de récupérer la date actuelle en utilisant la commande `date`. Voici un exemple de code :

```bash
current_date=`date +%Y-%m-%d`
echo $current_date
```

La commande `date +%Y-%m-%d` renvoie la date au format "année-mois-jour" (par exemple 2021-12-31). Ensuite, nous utilisons la variable `$current_date` pour stocker cette valeur et l'afficher à l'aide de la commande `echo`. Vous pouvez également modifier le format de la date en utilisant d'autres options disponibles pour la commande `date`. Par exemple, `date +%d/%m/%Y` renvoie la date au format "jour/mois/année" (par exemple 31/12/2021).

Vous pouvez également spécifier une date spécifique à convertir en chaîne en utilisant la commande `date -d`. Voici un exemple :

```bash
specific_date=`date -d "6 days ago" +%Y-%m-%d`
echo $specific_date
```

Ce code affiche la date d'il y a 6 jours au format "année-mois-jour". Vous pouvez modifier l'expression "6 days ago" en utilisant d'autres expressions telles que "2 weeks ago" ou "next Monday" pour récupérer une date spécifique.

Voici d'autres options utiles pour la commande `date` qui peuvent vous aider à personnaliser la conversion de date en chaîne :

- `%a` : jour de la semaine en trois lettres (ex: Mon)
- `%A` : jour de la semaine en entier (ex: Monday)
- `%b` : mois en trois lettres (ex: Jan)
- `%B` : mois en entier (ex: January)
- `%R` : heure et minute au format 24h (ex: 15:45)
- `%T` : heure, minute et seconde au format 24h (ex: 15:45:30)

## Deep Dive

La commande `date` utilise les informations de time zone de la variable `$TZ` pour afficher la date. Par défaut, cette variable est configurée pour utiliser la time zone du système. Cependant, vous pouvez la modifier en utilisant la commande `export` pour spécifier une time zone différente. Par exemple, `export TZ=EST` changera la time zone pour Eastern Standard Time.

Vous pouvez également utiliser cette commande pour afficher la date dans un fuseau horaire différent en utilisant l'option `-u`, qui spécifie le temps universel coordonné (UTC). Voici un exemple :

```bash
utc_date=`date -u`
echo $utc_date
```

Cela peut être utile dans les cas où vous avez besoin de convertir une date en UTC avant de la convertir en chaîne.

## Voir aussi

- [Documentation officielle pour la commande `date`](https://www.gnu.org/software/coreutils/manual/html_node/date-invocation.html#date-invocation)
- [Article avec plus d'exemples sur la conversion de dates en chaîne en Bash](https://www.baeldung.com/linux/convert-date-to-string-bash)