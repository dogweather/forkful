---
title:    "Go: Comparer deux dates"
keywords: ["Go"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fr/go/comparing-two-dates.md"
---

{{< edit_this_page >}}

# Pourquoi

Les dates sont un aspect important de la programmation, car elles nous permettent de suivre le temps et de traiter des informations sur des périodes spécifiques. Comparer deux dates peut être utile pour de nombreuses tâches, telles que le tri des données, la planification d'événements ou la création de fonctionnalités basées sur le temps. Dans cet article, nous allons voir comment comparer facilement et efficacement deux dates en utilisant Go.

## Comment faire

Pour comparer deux dates en Go, nous utilisons la fonction `Before()` ou `After()` de la structure `time.Time`. Cette fonction prend une autre date en argument et renvoie un booléen indiquant si la date en cours est antérieure ou postérieure à l'autre date.

```
Go
date1 := time.Date(2020, time.April, 15, 0, 0, 0, 0, time.UTC)
date2 := time.Date(2020, time.April, 20, 0, 0, 0, 0, time.UTC)

if date1.After(date2) {
  fmt.Println("La date 1 est postérieure à la date 2")
}

if date1.Before(date2) {
  fmt.Println("La date 1 est antérieure à la date 2")
}
```

Dans cet exemple, la fonction `After(date2)` renverra `true`, car la date 1 est bien postérieure à la date 2.

Nous pouvons également utiliser la fonction `Equal()` pour vérifier si deux dates sont égales.

```
Go
if date1.Equal(date2) {
  fmt.Println("Les dates sont égales")
}
```

Le format de date utilisé dans cet exemple est `YYYY, Month, Day, Hour, Minute, Second, Nanosecond, TimeZone`. Vous pouvez ajuster les valeurs pour correspondre à vos besoins.

## Plongée en profondeur

Il est important de noter que lors de la comparaison de dates, les fuseaux horaires doivent être pris en compte. La fonction `After()` et `Before()` utilisent le fuseau horaire spécifié pour chaque date dans sa structure `Time`. Si les deux dates ont des fuseaux horaires différents, la comparaison peut être faussée. Il est donc conseillé d'utiliser le même fuseau horaire pour toutes les dates comparées.

De plus, la fonction `Equal()` compare non seulement les valeurs de date, mais aussi les fuseaux horaires. Cela peut entraîner des résultats inattendus si le fuseau horaire n'est pas pris en compte lors de la déclaration des dates.

## Voir aussi

Pour plus d'informations sur les dates en Go, consultez la documentation officielle de la bibliothèque `time` (https://golang.org/pkg/time/) et la documentation sur la comparaison de dates (https://golang.org/doc/go1#comparing_dates). Vous pouvez également consulter les documents sur les fuseaux horaires en Go (https://golang.org/doc/go1#time_zones).

N'hésitez pas à explorer et à expérimenter avec ces concepts pour améliorer vos compétences en programmation Go. Bon codage !