---
title:                "Gleam: Comparer deux dates"
simple_title:         "Comparer deux dates"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/gleam/comparing-two-dates.md"
---

{{< edit_this_page >}}

#Pourquoi

Pourquoi comparer deux dates en utilisant Gleam? Lorsque vous travaillez avec des données temporelles dans votre code, il est important de pouvoir comparer ces dates pour effectuer des analyses, des calculs ou des opérations logiques. Gleam offre une syntaxe simple et efficace pour effectuer ces comparaisons, en minimisant le risque d'erreurs et en garantissant des résultats précis.

#Comment faire

Pour comparer deux dates en Gleam, nous pouvons utiliser la fonction `DateTime.compare/2`. Cette fonction prend deux arguments de type `DateTime` et renvoie un résultat de type `Order`. Jetons un coup d'œil à un exemple de code utilisant cette fonction:

```Gleam
let date_1 = DateTime.from_parts(2000, 1, 1, 0, 0, 0, 0)
let date_2 = DateTime.from_parts(2020, 1, 1, 0, 0, 0, 0)

let order = DateTime.compare(date_1, date_2)

case order {
  Lt -> "date_1 est avant date_2"
  Eq -> "date_1 est égale à date_2"
  Gt -> "date_1 est après date_2"
}
```

Dans cet exemple, nous créons deux objets `DateTime` représentant les dates du 1er janvier 2000 et du 1er janvier 2020. Ensuite, nous utilisons la fonction `DateTime.compare/2` pour comparer ces deux dates et stocker le résultat dans la variable `order`. Nous utilisons ensuite une expression `case` pour vérifier le résultat et imprimer un message approprié en fonction du résultat.

La sortie de ce code serait "date_1 est avant date_2" car 2000 est une année antérieure à 2020.

#Plongée en profondeur

En plus de la fonction `DateTime.compare/2`, Gleam propose également d'autres outils pour comparer des dates. Par exemple, la fonction `DateTime.is_same_day/2` permet de vérifier si deux dates sont le même jour ou non, tandis que la fonction `DateTime.difference_in_days/2` renvoie le nombre de jours entre deux dates.

Il est également possible de comparer des dates en tenant compte du fuseau horaire en utilisant la bibliothèque externe `timezonedb`, qui fournit des fonctions pour convertir les dates dans différents fuseaux horaires et effectuer des comparaisons en conséquence.

#Voir aussi

- [Documentation Gleam sur les dates et les heures](https://gleam.run/documentation/std/datetime/)
- [Exemple de comparaison de dates avec Gleam](https://github.com/gleam-lang/example-datetime-compare)
- [Bibliothèque externe `timezonedb` pour les comparaisons de dates avec les fuseaux horaires](https://docs.rs/timezonedb/0.14.0/timezonedb/)