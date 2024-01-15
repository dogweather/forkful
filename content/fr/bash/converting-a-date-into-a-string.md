---
title:                "Conversion d'une date en chaîne de caractères"
html_title:           "Bash: Conversion d'une date en chaîne de caractères"
simple_title:         "Conversion d'une date en chaîne de caractères"
programming_language: "Bash"
category:             "Bash"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/bash/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Pourquoi

La conversion d'une date en chaîne de caractères peut être utile lorsque vous avez besoin de formater la date dans un format spécifique pour votre script Bash ou pour afficher la date dans un certain format pour les utilisateurs. Cela peut être particulièrement pratique dans les tâches de traitement de données et de génération de rapports.

## Comment faire

Pour convertir une date en chaîne de caractères dans Bash, vous pouvez utiliser la commande `date` suivie du format souhaité entre guillemets. Voici un exemple de code:

```Bash
date "+%d-%m-%Y"
```
Exécutez ce code et vous obtiendrez la date actuelle au format jour-mois-année. Vous pouvez également inclure l'heure et le fuseau horaire dans le format en ajoutant des options, telles que `+%H:%M %Z`.

Pour obtenir une liste complète des options de format disponibles, vous pouvez utiliser la commande `man date` pour accéder au manuel de la commande. Vous pouvez également consulter cet article pour une liste plus détaillée des options de format couramment utilisées.

## Plongée plus profonde

La commande `date` peut également être utilisée pour convertir une date en timestamp (nombre de secondes écoulées depuis le 1er janvier 1970) ou inversement, en spécifiant le format `%s` pour le timestamp ou `%Y-%m-%d %H:%M:%S` pour la date et l'heure. De plus, vous pouvez modifier la date en utilisant la syntaxe suivante :

```Bash
date -d "2 days ago"
```
Cela affichera la date d'il y a 2 jours à partir de la date actuelle au format standard. Vous pouvez également utiliser des expressions telles que "next Monday", "last week", "now + 2 hours", etc. pour personnaliser la date que vous souhaitez afficher.

## Voir aussi

- [Documentation officielle de la commande date](https://www.gnu.org/software/coreutils/manual/html_node/date-invocation.html)
- [Guide de référence pour les options de format de date](https://www.computerhope.com/unix/udate.htm)
- [Unix Timestamp Converter pour convertir facilement entre une date et un timestamp](https://www.unixtimestamp.com/index.php)