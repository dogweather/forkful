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

## Pourquoi

Si vous êtes comme moi, vous devez souvent travailler avec des dates dans vos scripts shell. Parfois, vous pourriez avoir besoin de comparer deux dates pour voir si elles sont égales, ou si l'une est antérieure ou postérieure à l'autre. Dans cet article, nous allons voir comment comparer deux dates avec Fish Shell.

## Comment faire

Pour comparer deux dates avec Fish Shell, nous allons utiliser la commande `date` et la fonction `math`. Voici un exemple de script qui compare deux dates :

```Fish Shell
set date1 (date -I)
set date2 2021-03-15
if math "$date1 > $date2"
    echo "La date 1 est après la date 2"
else if math "$date1 == $date2"
    echo "Les deux dates sont égales"
else
    echo "La date 1 est avant la date 2"
end
```

En utilisant la commande `date -I`, nous obtenons la date actuelle au format YYYY-MM-DD et nous la stockons dans la variable `date1`. Ensuite, nous définissons une autre date dans la variable `date2`. La commande `math` nous permet de comparer ces deux dates en utilisant les opérateurs de comparaison `>`, `==` et `<`. En fonction du résultat, nous affichons un message correspondant.

Voici un exemple de sortie possible en utilisant le script ci-dessus :

```
La date 1 est après la date 2
```

## Plongée en profondeur

La fonction `math` utilise la syntaxe du langage de programmation C pour évaluer les expressions mathématiques. Cela signifie que nous pouvons utiliser non seulement les opérateurs de comparaison, mais aussi les opérateurs arithmétiques tels que `+`, `-`, `*` et `/` pour manipuler les dates avant de les comparer.

Par exemple, si nous voulons vérifier si une date est entre deux autres dates, nous pouvons utiliser la commande suivante :

```Fish Shell
if math "$date1 > $date_debut" and math "$date2 < $date_fin"
    echo "La date se situe entre $date_debut et $date_fin"
else
    echo "La date n'est pas entre $date_debut et $date_fin"
end
```

Nous pouvons également utiliser la fonction `string` pour convertir une date au format texte en format numérique afin de pouvoir la comparer avec une autre date numérique. Par exemple :

```Fish Shell
set date1 (date -I)
set date2 2021-03-15
set date_fin (string -r $date2)
if math "$date1 > $date_fin"
    echo "La date 1 est après la date 2"
else
    echo "La date 1 est avant ou égale à la date 2"
end
```

En utilisant la fonction `string -r`, nous convertissons la date `2021-03-15` en `20210315` pour pouvoir la comparer avec la date numérique dans la variable `date1`.

## Voir aussi

- Documentation officielle de Fish Shell : https://fishshell.com/docs/current/index.html
- Articles sur Fish Shell sur le blog de DigitalOcean : https://www.digitalocean.com/community/tags/fish-shell?type=tutorials