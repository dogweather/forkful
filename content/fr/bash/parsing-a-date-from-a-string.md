---
title:                "Analyser une date à partir d'une chaîne de caractères"
html_title:           "Bash: Analyser une date à partir d'une chaîne de caractères"
simple_title:         "Analyser une date à partir d'une chaîne de caractères"
programming_language: "Bash"
category:             "Bash"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/bash/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## Qu'est-ce que c'est et pourquoi?
Le "parseur de date de chaîne" est un outil qui permet aux programmeurs de prendre une date sous forme de texte (chaîne de caractères) et de la convertir en un format compréhensible pour l'ordinateur. Les programmeurs ont souvent besoin de faire cela lorsqu'ils travaillent avec des données de différentes sources qui ne sont pas toutes au même format de date.

## Comment:
Voici un exemple simple pour montrer comment utiliser un parseur de date de chaîne en Bash:

```Bash
# Déclarer une date sous forme de chaîne de caractères
my_date="2021-04-15"

# Convertir la date en format UNIX (nombre de secondes écoulées depuis le 1er janvier 1970)
my_timestamp=$(date -d "$my_date" +%s)

# Afficher le résultat
echo "La date $my_date correspond à $my_timestamp secondes depuis 1970."
```
**Output:**
```
La date 2021-04-15 correspond à 1618444800 secondes depuis 1970.
```

## Plongée en profondeur:
Le besoin de convertir des dates en formats compatibles avec l'ordinateur a toujours existé, mais il a été exacerbé avec la popularité croissante des données en ligne en temps réel. Avant la création de parseurs de date de chaîne, les programmeurs devaient écrire leur propre code pour chaque format de date spécifique, ce qui rendait leur travail plus compliqué et sujet aux erreurs. De nos jours, il existe plusieurs alternatives au Bash, telles que Python ou JavaScript, pour gérer la conversion de dates.

## À voir aussi:
- [Documentation officielle de GNU Date](https://www.gnu.org/software/coreutils/manual/html_node/date-invocation.html)
- [Guide pour la manipulation de dates en Bash](https://www.lifewire.com/date-command-examples-4087371)