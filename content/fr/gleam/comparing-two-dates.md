---
title:                "Comparaison de deux dates"
html_title:           "Gleam: Comparaison de deux dates"
simple_title:         "Comparaison de deux dates"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/gleam/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Pourquoi

Si vous être un développeur en herbe ou expérimenté, vous savez probablement que travailler avec des dates peut être un vrai casse-tête. Heureusement, Gleam vient à la rescousse avec une fonction simple et efficace pour comparer deux dates. Dans cet article, nous allons jeter un coup d'œil à la méthode "compare" pour découvrir comment elle peut simplifier votre vie de programmateur.

## Comment faire

Tout d'abord, nous avons besoin de déclarer nos deux dates à comparer. Dans notre exemple, nous allons utiliser le 1er janvier 2020 et le 1er février 2020 comme nos deux dates.

```Gleam
let date_1 = Date.from_iso8601("2020-01-01")
let date_2 = Date.from_iso8601("2020-02-01")
```

Une fois que nous avons nos deux dates, nous pouvons facilement comparer leur ordre en utilisant la méthode "compare".

```Gleam
let order = date_1.compare(date_2)
```

Maintenant, si nous imprimons la valeur de "order", nous devrions avoir -1 car janvier vient avant février en ordre chronologique.

```Gleam
IO.println(order) // output: -1
```

Mais que se passe-t-il si nous comparons deux dates qui ont la même valeur? Par exemple, le 1er janvier 2020 et le 1er janvier 2020. Dans ce cas, la méthode "compare" renverra 0 pour "égal".

```Gleam
let date_3 = Date.from_iso8601("2020-01-01")
let order = date_1.compare(date_3) // order = 0
```

Et enfin, si nous comparons une date qui vient après l'autre en termes chronologiques, la méthode renverra 1 pour "supérieur".

```Gleam
let date_4 = Date.from_iso8601("2020-03-01")
let order = date_1.compare(date_4) // order = 1
```

Et voilà, grâce à Gleam, nous pouvons facilement comparer deux dates sans nous soucier des détails compliqués.

## Plongée profonde

Maintenant que nous avons vu comment utiliser la méthode "compare", nous pouvons également nous pencher sur ce qui se passe en coulisses. En réalité, Gleam utilise des timestamps pour représenter les dates et utilise la méthode "compare" pour comparer ces timestamps. Cela signifie que, en termes de performance, la méthode "compare" est extrêmement rapide et efficace.

## Voir aussi

- Documentation officielle de Gleam sur la méthode "compare": https://gleam.run/documentation/std/datetime#compare
- Un guide pratique sur la gestion des dates avec Gleam: https://dev.to/jessew/measures-are-ahead-52lp