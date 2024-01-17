---
title:                "Comparer deux dates"
html_title:           "Fish Shell: Comparer deux dates"
simple_title:         "Comparer deux dates"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/fish-shell/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Qu'est-ce que c'est et pourquoi le faire?

Comparer deux dates en programmation consiste à comparer la valeur numérique de deux dates pour déterminer leur ordre chronologique. Les programmeurs le font souvent pour trier des données ou pour vérifier si une date est antérieure ou ultérieure à une autre.

## Comment faire:

Voici un exemple de code en Fish Shell pour comparer deux dates:

```
# Définir les variables des deux dates à comparer
set date1 20210215
set date2 20210315

if [ $date1 -eq $date2 ]
    echo "Les deux dates sont identiques"
else if [ $date1 -lt $date2 ]
    echo "La date 1 est antérieure à la date 2"
else
    echo "La date 1 est ultérieure à la date 2"
end
```

Voici le résultat de l'exécution du code:

```
La date 1 est antérieure à la date 2
```

## Plongée en profondeur:

Cette méthode de comparaison de dates est basée sur le format numérique YYYYMMJJ, où Y représente l'année, M le mois et J le jour. Cela provient de la méthode utilisée par le système Unix pour stocker et comparer les dates. 
Cependant, il existe d'autres méthodes pour comparer les dates, telles que l'utilisation de fonctions spécifiques au langage de programmation, comme la fonction `strcmp()` en C.

## À voir également:

Vous pouvez en apprendre davantage sur la comparaison de dates en consultant les sources suivantes:

- Documentation officielle de la commandee `test` en Fish Shell: http://fishshell.com/docs/current/cmds/test.html
- Tutoriel sur la manipulation de dates en Fish Shell: https://devhints.io/fish
- Exemple de comparaison de dates en utilisant la fonction `strcmp()` en C: https://www.geeksforgeeks.org/strcmp-in-c-cpp/