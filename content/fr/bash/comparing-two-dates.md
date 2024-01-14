---
title:                "Bash: Comparer deux dates"
simple_title:         "Comparer deux dates"
programming_language: "Bash"
category:             "Bash"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/bash/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Pourquoi

La comparaison de deux dates peut être une tâche utile dans la programmation Bash pour diverses raisons, telles que la vérification de la validité d'une date saisie par l'utilisateur ou la gestion de données chronologiques dans des scripts. Dans cet article, nous allons vous montrer comment le faire en utilisant la syntaxe Bash.

## Comment faire

Pour comparer deux dates en Bash, nous allons utiliser la commande `date` et les opérateurs de comparaison disponibles en Bash. Voici un exemple de code pour comparer deux dates et afficher un message en fonction du résultat :

```Bash
#! /bin/bash
date1="2020-01-01" # première date
date2="2020-01-15" # deuxième date

if [[ "$date1" < "$date2" ]]; then
  echo "La date 1 est antérieure à la date 2."
elif [[ "$date1" == "$date2" ]]; then
  echo "Les deux dates sont identiques."
else
  echo "La date 1 est postérieure à la date 2."
fi
```

Dans cet exemple, nous avons défini deux variables contenant des dates au format "année-mois-jour". Ensuite, nous les avons comparées en utilisant les opérateurs `<` et `==` pour déterminer si la première date est antérieure, égale ou postérieure à la deuxième date. Selon le résultat, un message approprié est affiché à l'utilisateur.

Vous pouvez également utiliser la commande `date` pour récupérer la date actuelle et la comparer avec une date saisie par l'utilisateur :

```Bash
#! /bin/bash
today=$(date +"%Y-%m-%d") # date actuelle
echo "Veuillez saisir une date au format YYYY-MM-DD : "
read user_date # saisie de la date par l'utilisateur

if [[ "$user_date" < "$today" ]]; then
  echo "La date saisie est antérieure à la date actuelle."
elif [[ "$user_date" == "$today" ]]; then
  echo "La date saisie est la même que la date actuelle."
else
  echo "La date saisie est postérieure à la date actuelle."
fi
```

Dans cet exemple, nous avons utilisé la commande `date` avec l'option `+%Y-%m-%d` pour récupérer la date actuelle et la stocker dans la variable `today`. Ensuite, grâce à la commande `read`, nous avons demandé à l'utilisateur de saisir une date au même format et l'avons stockée dans la variable `user_date`. Enfin, nous avons comparé les deux dates et affiché un message en conséquence.

## Plongée en profondeur

Lorsque vous comparez des dates en Bash, il est important de prendre en compte le format des dates et l'utilisation d'opérateurs appropriés. Si vous comparez des dates au format "jour-mois-année", vous devez utiliser l'opérateur `>` pour déterminer si une date est postérieure à une autre. De plus, si vous utilisez l'opérateur `==` pour comparer des dates, elles doivent être dans le même format pour être identiques.

De plus, il est également possible de comparer des dates avec une précision plus grande que le jour en utilisant des options supplémentaires avec la commande `date` comme `+%s` pour le timestamp Unix ou `+%j` pour le jour de l'année. Cela peut être utile pour les comparaisons plus complexes impliquant des heures, des minutes et des secondes.

## Voir aussi

- [Documentation officielle de Bash](https://www.gnu.org/software/bash/)
- [Guide de référence pour la commande date en Bash](https://www.man7.org/linux/man-pages/man1/date.1.html)
- [Tutoriel pour débutants en Bash](https://devhint.io/fr/blog/2018-08-14-bash-scripting-les-elements-de-base/)