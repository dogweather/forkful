---
title:                "Comparer deux dates"
html_title:           "Clojure: Comparer deux dates"
simple_title:         "Comparer deux dates"
programming_language: "Bash"
category:             "Bash"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/bash/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Qu'est-ce et Pourquoi ?

Comparer deux dates consiste à déterminer quelle date est la plus récente ou si elles sont identiques. Les programmeurs font cela pour des opérations comme le tri, le filtrage ou le calcul de durées.

## Comment faire :

Pour comparer deux dates en Bash, utilisez l'opérateur "-gt" (plus grand que), "-lt" (moins que) ou "-eq" (égal à). L'astuce consiste à formater les dates en format AAAAMMJJ avant de les comparer. Voici un exemple:

```Bash
date1=$(date -d"2021-12-01" +"%Y%m%d")
date2=$(date -d"2022-01-01" +"%Y%m%d")

if [ $date1 -gt $date2 ]; then
    echo "date1 est plus récente que date2"
elif [ $date1 -lt $date2 ]; then
    echo "date2 est plus récente que date1"
else
    echo "Les deux dates sont identiques"
fi
```
La sortie de ces commandes serait :

```Bash
date2 est plus récente que date1
```

## Deep Dive :

Historiquement, comparer des dates en Bash n'était pas une tâche facile car Bash ne gère pas nativement les dates. Cela a changé avec l'introduction de la commande `date`.

En termes d'alternatives, vous pouvez aussi utiliser des outils comme awk, Perl ou Python pour comparer des dates, qui offrent plus de flexibilité mais avec un coût d'apprentissage plus élevé et une moins bonne intégration avec les scripts Bash.

Concernant les détails de mise en œuvre, il est important de se rappeler que les opérateurs de comparaison de Bash fonctionnent avec des nombres entiers. C'est la raison pour laquelle nous convertissons les dates en format AAAAMMJJ.

## Voir Aussi :

Pour aller plus loin dans la programmation Bash:

- [GNU Bash Manual](https://www.gnu.org/software/bash/manual/bash.html)
- [Guide avancé Bash Scripting](http://tldp.org/LDP/abs/html/)
- [StackOverflow: Comparing Dates in Bash](https://stackoverflow.com/questions/3430181/comparing-dates-in-a-shell-script)