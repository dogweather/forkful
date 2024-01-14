---
title:    "Bash: Comparaison de deux dates"
keywords: ["Bash"]
---

{{< edit_this_page >}}

## Pourquoi

La comparaison de deux dates est une tâche courante dans la programmation Bash, que ce soit pour vérifier la validité d'une entrée utilisateur ou pour effectuer des opérations sur des fichiers basées sur leur date de création. Savoir comment comparer des dates correctement est donc une compétence importante pour tout programmeur Bash.

## Comment faire

Tout d'abord, pour comparer deux dates, nous devons nous assurer qu'elles sont au bon format. En Bash, la commande `date` peut être utilisée pour obtenir la date et l'heure actuelles dans différents formats. Par exemple, pour obtenir la date au format `JJ/MM/AAAA`, nous pouvons utiliser `date +"%d/%m/%Y"`.

Ensuite, nous pouvons stocker ces valeurs dans des variables et les comparer en utilisant les opérateurs de comparaison tels que `==` (égal), `!=` (différent), `<` (inférieur), `>` (supérieur), `<=` (inférieur ou égal) et `>=` (supérieur ou égal).

En voici un exemple de comparaison de dates et de sortie correspondante :

```Bash
# Stocker les dates dans des variables
date1=$(date -d "10 days ago" +"%d/%m/%Y")
date2=$(date -d "3 days ago" +"%d/%m/%Y")

# Comparer les dates
if [[ $date1 > $date2 ]]; then
  echo "La première date est postérieure à la deuxième date."
else
  echo "La première date est antérieure à la deuxième date."
fi

# Sortie : La première date est postérieure à la deuxième date.
```

Pour des comparaisons plus complexes, nous pouvons également utiliser les commandes `date` ou `awk` pour extraire des informations spécifiques des dates, telles que le jour de la semaine ou le mois, et les comparer ensuite.

## Plongée en profondeur

Les dates peuvent être comparées selon différentes unités de temps telles que les jours, les mois ou les années. Cependant, il est important de noter que la comparaison de dates peut également prendre en compte l'heure dans certains cas. Par exemple, si nous voulons comparer deux fichiers basés sur leur date de modification, il peut être utile de définir la précision à l'heure afin de ne pas confondre différents fichiers créés le même jour.

De plus, en plus des opérateurs de comparaison, il existe également différentes commandes telles que `find` ou `stat` qui peuvent être utiles pour comparer les dates des fichiers dans un script Bash.

## Voir aussi

- [Documentation officielle de Bash](https://www.gnu.org/software/bash/)
- [Comparaison de variables avec Bash](https://www.tldp.org/LDP/Bash-Beginners-Guide/html/sect_07_01.html)
- [Commande `date` dans Bash](https://www.geeksforgeeks.org/date-command-linux-examples/)