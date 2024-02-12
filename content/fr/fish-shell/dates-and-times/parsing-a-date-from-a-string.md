---
title:                "Analyser une date depuis une chaîne de caractères"
date:                  2024-02-03T19:14:00.125677-07:00
model:                 gpt-4-0125-preview
simple_title:         "Analyser une date depuis une chaîne de caractères"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/fish-shell/parsing-a-date-from-a-string.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Quoi & Pourquoi ?
L'analyse d'une date à partir d'une chaîne de caractères implique d'extraire les informations de date encodées au sein de chaînes et de les convertir en un format structuré que les environnements de programmation peuvent reconnaître et manipuler. Les programmeurs font cela pour permettre des opérations telles que la comparaison de dates, l'arithmétique, le formatage et la localisation, qui sont essentielles pour gérer efficacement la planification, les horodatages et les données historiques dans les logiciels.

## Comment faire :
Dans Fish Shell, il n'y a pas de commandes intégrées spécifiquement conçues pour l'analyse de dates à partir de chaînes. À la place, vous vous appuyez sur des utilitaires externes comme `date` (disponible sous Linux et macOS) ou utilisez des outils tiers populaires tels que `GNU date` pour une analyse plus complexe. Voici comment s'y prendre :

**Utiliser `date` avec Fish :**

Pour analyser une chaîne de date au format "AAAA-MM-JJ", vous pouvez utiliser la commande `date` avec l'option `-d` (ou `--date` pour GNU date) suivie de la chaîne. L'option `+` est utilisée pour formater la sortie.

```fish
set date_str "2023-04-01"
date -d $date_str +"%A, %d %B %Y"
# Sortie : Saturday, 01 April 2023
```

Pour macOS (qui nécessite un format différent pour les drapeaux `-j` et `-f`) :

```fish
set date_str "2023-04-01"
date -j -f "%Y-%m-%d" $date_str +"%A, %d %B %Y"
# Sortie : Saturday, 01 April 2023
```

**Utiliser GNU `date` pour une analyse complexe :** 

GNU `date` est plus flexible avec les formats de chaînes. Il peut détecter automatiquement de nombreux formats de chaînes de dates communs sans spécifier explicitement le format d'entrée :

```fish
set complex_date_str "April 1, 2023 14:00"
date -d "$complex_date_str" '+%Y-%m-%d %H:%M:%S'
# Sortie : 2023-04-01 14:00:00
```

Cependant, lors du travail avec des chaînes de dates qui peuvent ne pas être automatiquement reconnues ou lorsque un contrôle précis sur le format d'entrée est nécessaire, spécifier le format d'entrée avec GNU `date` n'est pas directement pris en charge. Dans de tels cas, envisagez de prétraiter la chaîne ou d'utiliser un autre outil conçu pour des routines d'analyse de dates plus complexes.
