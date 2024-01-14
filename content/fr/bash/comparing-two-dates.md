---
title:                "Bash: Comparer deux dates"
programming_language: "Bash"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/bash/comparing-two-dates.md"
---

{{< edit_this_page >}}

# Pourquoi

Déterminer si deux dates sont identiques ou si l'une est antérieure ou postérieure à l'autre est une tâche courante en programmation. Cela peut être utile pour trier des données ou pour effectuer des calculs basés sur des dates. Dans cet article, nous allons explorer comment comparer deux dates en utilisant le langage de programmation Bash.

# Comment faire

Pour comparer deux dates en Bash, nous allons utiliser la commande `date` et les opérateurs de comparaison `<` et `>`. Voici un exemple de code qui compare deux dates et affiche le résultat :

```Bash
date1="2021-04-10"
date2="2021-04-15"

if [ "$date1" \< "$date2" ]; then
  echo "$date1 est antérieure à $date2"
elif [ "$date1" \> "$date2" ]; then
  echo "$date1 est postérieure à $date2"
else
  echo "Les deux dates sont identiques"
fi
```

Dans cet exemple, nous avons défini deux variables représentant des dates au format `AAAA-MM-JJ`. Ensuite, nous utilisons une structure `if` pour comparer ces dates en utilisant l'opérateur `<` pour déterminer si la première date `date1` est antérieure à la seconde `date2` et l'opérateur `>` pour déterminer si la première date est postérieure à la seconde. Si aucune de ces conditions n'est vérifiée, cela signifie que les deux dates sont identiques.

L'exemple ci-dessus ne couvre que les cas où les deux dates sont dans le même mois et la même année. Pour comparer des dates qui ne sont pas dans le même mois ou la même année, nous pouvons utiliser la commande `date` pour convertir ces dates en timestamp (nombre de secondes écoulées depuis le 1er janvier 1970) et comparer ces valeurs. Voici un autre exemple de code :

```Bash
date1="2020-12-25"
date2="2021-04-01"

timestamp1=$(date -d "$date1" +%s)
timestamp2=$(date -d "$date2" +%s)

if [ "$timestamp1" -lt "$timestamp2" ]; then
  echo "$date1 est antérieur à $date2"
elif [ "$timestamp1" -gt "$timestamp2" ]; then
  echo "$date1 est postérieur à $date2"
else
  echo "Les deux dates sont identiques"
fi
```

Dans cet exemple, nous utilisons la commande `date` avec l'option `-d` pour convertir les dates en timestamp et la commande `date` avec l'option `+%s` pour afficher ces timestamps. Ensuite, nous utilisons les opérateurs de comparaison `-lt` (inférieur à) et `-gt` (supérieur à) pour comparer ces timestamps. 

# Plongée en profondeur

Il est important de noter que les opérateurs de comparaison `<` et `>` fonctionnent uniquement avec des nombres. C'est pourquoi nous devons convertir les dates en timestamp avant de les comparer. Cela peut sembler un peu compliqué, mais cela garantit une précision et une fiabilité maximale lors de la comparaison de dates.

De plus, il est important de s'assurer que les dates sont dans le bon format. Dans les exemples ci-dessus, nous avons utilisé le format `AAAA-MM-JJ` car c'est le format standard pour les dates en Bash. Si les dates sont dans un format différent, vous devrez peut-être les convertir avant de les comparer.

# Voir aussi

- [Bash Reference Manual](https://www.gnu.org/software/bash/manual/bash.html)
- [Comparaison de dates en Shell](https://stackoverflow.com/questions/8211566/how-do-i-compare-dates-in-shell-scripts)
- [Convertir une date en timestamp en Bash](https://www.baeldung.com/linux/date-command-bash)