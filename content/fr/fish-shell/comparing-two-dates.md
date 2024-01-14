---
title:                "Fish Shell: Comparer deux dates"
programming_language: "Fish Shell"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/fish-shell/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Pourquoi

La comparaison de deux dates est une tâche courante dans la programmation, qui peut être particulièrement utile lors de la gestion de données temporelles ou lors de la mise en place de fonctionnalités basées sur la date. Cet article vous montrera comment réaliser facilement cette comparaison en utilisant Fish Shell.

## Comment faire

Pour comparer deux dates en utilisant Fish Shell, nous devrons d'abord les convertir au format UNIX Epoch, qui est un nombre entier représentant le nombre de secondes écoulées depuis le 1er janvier 1970. Voici un exemple de code pour convertir une date en format Epoch :

```
# Définir la date à comparer
set date_1 (date +%s)
set date_2 "2020-01-01"

# Convertir la date au format Epoch
set epoch_date_2 (date -f "%Y-%m-%d" -i $date_2 +%s)

# Comparer les deux dates
if test $date_1 -gt $epoch_date_2
    echo "Date 1 est plus récente que Date 2"
else if test $date_1 -lt $epoch_date_2
    echo "Date 1 est plus ancienne que Date 2"
else 
    echo "Les deux dates sont identiques"
end
```

Dans cet exemple, nous utilisons la commande `date` pour obtenir la date du jour et nous la convertissons ensuite en format Epoch en utilisant l'option `-f` pour spécifier le format de la date et l'option `-i` pour spécifier la date à convertir. Enfin, nous comparons les deux dates en utilisant la commande `test` et affichons le résultat. 

## Deep Dive

L'utilisation de Fish Shell pour comparer des dates est simple et pratique, mais il y a également quelques astuces à connaître. Par exemple, si vous avez besoin de comparer des dates avec une précision supérieure à la seconde, vous pouvez utiliser la commande `strftime` pour formater les dates au format souhaité. De plus, en utilisant les variables d'environnement `BASE_DATE` et `BASE_TIME`, vous pouvez également définir une date et une heure de référence pour vos comparaisons de dates. Pour plus d'informations, vous pouvez consulter la documentation de Fish Shell sur les dates.

## Voir aussi

- [Documentation Fish Shell sur les dates](https://fishshell.com/docs/current/commands.html#date)
- [Guide des commandes Fish Shell](https://fishshell.com/docs/current/commands.html)
- [Exemples de scripts Fish Shell](https://github.com/fish-shell/fish-shell/wiki/Scripts)