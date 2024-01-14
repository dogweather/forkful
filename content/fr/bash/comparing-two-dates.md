---
title:    "Bash: Comparaison de deux dates"
keywords: ["Bash"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fr/bash/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Pourquoi

Lorsque vous créez des scripts Bash, il est souvent nécessaire de comparer deux dates pour prendre une décision basée sur leur différence. Que ce soit pour effectuer une sauvegarde régulière de vos fichiers ou pour vérifier si un certificat SSL a expiré, la comparaison de dates peut être un outil très utile.

## Comment faire

Pour comparer deux dates en Bash, il vous suffit d'utiliser la commande `date -d` suivie des deux dates à comparer, puis de spécifier comment vous souhaitez les comparer. Voici un exemple de script pour comparer la date actuelle avec une date donnée :

```Bash
#!/bin/bash

date1="2021-01-01" # Date à comparer
date2=$(date +"%F") # Date actuelle

if [ $(date -d "$date1" +%s) -lt $(date -d "$date2" +%s) ]
then
  echo "$date1 est antérieure à $date2"
else
  echo "$date1 est postérieure à $date2"
fi
```

Dans cet exemple, nous utilisons la commande `date -d` pour convertir les dates en format UNIX et les comparer en utilisant l'opérateur `-lt` (inférieur). Vous pouvez également utiliser des comparaisons telles que `-gt` (supérieur) ou `-eq` (égal).

## Plongez plus profondément

En comparant deux dates en Bash, il est important de comprendre comment le système d'exploitation les interprète. Le format de date le plus couramment utilisé en Bash est celui des "secondes depuis l'époque UNIX" (1er janvier 1970). Il est également possible de les convertir en "jours depuis l'époque UNIX" en ajoutant l'option `--iso-8601=seconds` à la commande `date -d`.

Il est également important d'être conscient des différences entre les dates locales et les dates universelles. La conversion en "jours depuis l'époque UNIX" peut être utile pour éviter les problèmes liés aux fuseaux horaires.

## Voir aussi

- [Documentation officielle de la commande `date`](https://www.gnu.org/software/coreutils/manual/html_node/date-invocation.html#date-invocation)
- [Guide pour utiliser la commande `date` en Bash](https://www.lifewire.com/date-command-linux-unix-4097048)
- [Plus d'informations sur le format de date UNIX](https://www.epochconverter.com/)