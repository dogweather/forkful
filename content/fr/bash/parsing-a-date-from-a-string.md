---
title:                "Analyser une date à partir d'une chaîne"
html_title:           "Clojure: Analyser une date à partir d'une chaîne"
simple_title:         "Analyser une date à partir d'une chaîne"
programming_language: "Bash"
category:             "Bash"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/bash/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

# Parsing d'une Date à partir d'une Chaîne en Bash

## Pourquoi et Quoi ?

Le "parsing" d'une date à partir d'une chaîne, c'est convertir une représentation textuelle d'une date en une structure de données gérable par le code. Les programmeurs le font pour manipuler et utiliser efficacement les informations temporelles dans leurs programmes.

## Comment faire :

Voici un exemple simple de comment vous pouvez réaliser ceci dans Bash :
```Bash
#!/bin/bash
dateString="2022-02-20"
dateParsed=$(date -d "$dateString" "+%Y%m%d")
echo $dateParsed
```
Lorsque vous exécutez ce script, vous obtiendrez le format de date converti :
```Bash
20220220
```

## Plongée Profonde

Historiquement, le parsing de dates a été une tâche ardue pour les programmeurs en raison de la diversité des formats de dates. Cependant, dans Bash modernes (GNU `date`), il existe une fonction intégrée pour parser une date à partir d'une chaîne.

Les alternatives à `date` comprennent les commandes `awk`, `sed` et `perl` que vous pouvez utiliser dans une chaîne de pipes pour parser des dates. Par exemple, vous pouvez utiliser `awk` pour split une date et la stocker dans des variables individuelles :

```Bash
#!/bin/bash
dateString="2022-02-20"
IFS="-"; read -a dateArray <<< "$dateString"
year=${dateArray[0]}
month=${dateArray[1]}
day=${dateArray[2]}
echo $year $month $day
```

En interne, `date -d` invoque la fonction `getdate`, qui supporte de nombreux formats de dates. Cependant, c'est une fonction locale et non recommandée pour une portabilité maximale.

## Voir Aussi

Les liens suivants fournissent des informations supplémentaires sur le parsing de date dans Bash :
- Guide Bash avancée : [Date et Heure](https://tldp.org/LDP/abs/html/timedate.html)
- GNU Core Utilities : [Date de GNU](https://www.gnu.org/software/coreutils/manual/html_node/date-invocation.html)
- Stack Overflow : [Convertir une date en Timestamp Unix](https://stackoverflow.com/questions/3249827/convert-from-date-to-timestamp-bash)