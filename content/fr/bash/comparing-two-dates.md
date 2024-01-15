---
title:                "Comparaison de deux dates"
html_title:           "Bash: Comparaison de deux dates"
simple_title:         "Comparaison de deux dates"
programming_language: "Bash"
category:             "Bash"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/bash/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Pourquoi

Si vous travaillez avec des dates dans vos scripts Bash, il est important de savoir comment les comparer pour effectuer des actions en fonction de leur relation. Cela peut être utile pour trier des fichiers par date, vérifier si une date est dans le passé ou le futur, ou encore pour créer des sauvegardes basées sur une plage de dates spécifique.

## Comment faire

La comparaison de dates en Bash peut sembler intimidante, mais avec les bonnes commandes, c'est en fait assez simple. Voici un exemple de code qui compare deux dates saisies par l'utilisateur :

```Bash
read -p "Entrez une date au format YYYY-MM-DD : " date1
read -p "Entrez une autre date au format YYYY-MM-DD : " date2
if [[ "$date1" > "$date2" ]]; then
  echo "La première date est plus récente que la seconde."
elif [[ "$date1" < "$date2" ]]; then
  echo "La seconde date est plus récente que la première."
else
  echo "Les deux dates sont identiques."
```

Dans cet exemple, nous utilisons la commande `read` pour saisir les dates et les stockons dans les variables `date1` et `date2`. Ensuite, nous utilisons le comparateur `>`, qui vérifie si la première date est plus grande que la seconde. Si c'est le cas, nous affichons un message en conséquence. Sinon, nous utilisons le comparateur `<` pour vérifier si la seconde date est plus petite que la première. Si c'est le cas, nous affichons un autre message. Sinon, cela signifie que les deux dates sont identiques et nous en informons l'utilisateur.

## Plongée en profondeur

Lors de la comparaison de dates en Bash, il est important d'utiliser un format standard tel que `YYYY-MM-DD` pour éviter toute confusion. De plus, il est également important de prendre en compte les années bissextiles lors de la comparaison de dates.

Pour vérifier si une année est bissextile, vous pouvez utiliser la commande `date` avec l'option `%Y`, qui renvoie l'année complète, et l'option `%m`, qui renvoie le mois. Si la date du 29 février existe, cela signifie que l'année est bissextile.

```Bash
if [[ $(date -d "$year-02-29" +"%Y%m") == $(date -d "$year-03-01" +"%Y%m") ]]; then
  echo "Cette année est bissextile."
else
  echo "Cette année n'est pas bissextile."
fi
```

Il existe également d'autres comparateurs que vous pouvez utiliser pour comparer les dates, tels que `==` pour vérifier si deux dates sont identiques, `!=` pour vérifier si elles sont différentes, `<=` pour vérifier si la première date est antérieure à la seconde, et `>=` pour vérifier si elle est postérieure.

## Voir aussi

- [Guide complet sur la manipulation de dates en Bash](https://www.cyberciti.biz/faq/linux-unix-formatting-dates-for-display/)
- [Documentation sur la commande `date` en Bash](https://linux.die.net/man/1/date)
- [Comparaison de chaînes en Bash](https://www.linuxjournal.com/content/comparing-strings-bash)