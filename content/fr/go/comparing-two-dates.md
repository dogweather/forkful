---
title:    "Go: Comparer deux dates"
keywords: ["Go"]
---

{{< edit_this_page >}}

## Pourquoi
Les dates jouent un rôle important dans la programmation, surtout lorsqu'il s'agit de comparer deux dates. Comprendre comment comparer deux dates peut aider les programmeurs à résoudre des problèmes tels que la vérification de la validité d'une date ou la détermination de la date la plus récente.

## Comment faire
Comparer deux dates en Go est assez simple. Tout d'abord, nous devons créer deux variables de type time.Time qui représentent les deux dates que nous voulons comparer.

```Go
date1 := time.Date(2020, time.January, 1, 0, 0, 0, 0, time.UTC)
date2 := time.Date(2019, time.December, 31, 0, 0, 0, 0, time.UTC)

```

Ensuite, nous pouvons utiliser la fonction After() et Before() pour vérifier si une date est avant ou après l'autre.

```Go
fmt.Println(date1.After(date2))
// Output: true
```

Nous pouvons également utiliser la fonction Equal() pour vérifier si les deux dates sont égales.

```Go
fmt.Println(date1.Equal(date2))
// Output: false
```

Dans l'exemple ci-dessus, nous avons comparé les dates en utilisant le fuseau horaire UTC. Si nous voulons utiliser un fuseau horaire différent, nous pouvons utiliser la fonction In() pour convertir les dates.

```Go
date1 := time.Date(2020, time.January, 1, 0, 0, 0, 0, time.UTC)
date2 := time.Date(2020, time.January, 1, 0, 0, 0, 0, time.FixedZone("EST", -5*60*60))

fmt.Println(date1.In(time.UTC).Equal(date2))
// Output: true
```

## Plongée en profondeur
Il est important de noter que les fonctions After(), Before() et Equal() utilisent la représentation interne des dates en Go pour effectuer les comparaisons. Cette représentation est basée sur la durée en nanosecondes depuis le 1er janvier 1970, heure UTC. Cela signifie que les comparaisons peuvent donner des résultats inattendus si nous ne prêtons pas attention.

Par exemple, si nous comparons deux dates avec des fuseaux horaires différents, les résultats peuvent être différents en fonction de l'heure actuelle.

```Go
date1 := time.Date(2020, time.January, 1, 12, 0, 0, 0, time.UTC)
date2 := time.Date(2020, time.January, 1, 12, 0, 0, 0, time.FixedZone("EST", -5*60*60))

fmt.Println(date1.Equal(date2))
// Output: false
```

Dans cet exemple, date1 et date2 représentent la même heure, mais la comparaison renvoie false car la zone horaire actuelle est différente.

## Voir aussi
- [Documentation officielle de Go sur la manipulation des dates](https://golang.org/pkg/time/)
- [Tutoriel vidéo sur la comparaison de dates en Go](https://www.youtube.com/watch?v=3EcdzH7Btps)
- [Exemples de code pour comparer des dates en Go](https://golangbyexample.com/compare-dates-go/)