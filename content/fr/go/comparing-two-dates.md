---
title:                "Comparaison de deux dates"
html_title:           "Go: Comparaison de deux dates"
simple_title:         "Comparaison de deux dates"
programming_language: "Go"
category:             "Go"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/go/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Pourquoi

Vous êtes-vous déjà demandé comment vous pouvez comparer deux dates en programmation ? Que vous cherchiez à calculer une différence de temps ou à vérifier si une date est antérieure à une autre, il est utile de savoir comment effectuer cette opération en utilisant le langage de programmation Go.

## Comment faire

La comparaison de deux dates en utilisant Go est assez simple. Tout d'abord, vous devez créer deux variables de type `time.Time`, qui représenteront vos dates. Ensuite, vous pouvez utiliser l'une des fonctions de comparaison intégrées de Go pour effectuer la comparaison.

```Go
import "time"

// Créez deux dates
date1 := time.Date(2020, time.January, 1, 0, 0, 0, 0, time.UTC)
date2 := time.Date(2021, time.January, 1, 0, 0, 0, 0, time.UTC)

// Comparez les dates en utilisant la fonction Before
if date1.Before(date2) {
  fmt.Println("La date 1 est antérieure à la date 2")
} else {
  fmt.Println("La date 2 est antérieure à la date 1")
}

// Comparez les dates en utilisant la fonction Equal
if date1.Equal(date2) {
  fmt.Println("Les dates sont identiques")
} else {
  fmt.Println("Les dates sont différentes")
}
```

La sortie de ce code sera :

```
La date 1 est antérieure à la date 2
Les dates sont différentes
```

Comme vous pouvez le voir, il est également possible d'utiliser la fonction `Equal` pour vérifier si deux dates sont identiques ou non.

## Plongée en profondeur

Go utilise des valeurs de type `time.Time` pour représenter les dates et les heures. Ces valeurs ont de nombreuses fonctions utiles pour effectuer des opérations de comparaison. Par exemple, vous pouvez utiliser les fonctions `Before` et `After` pour vérifier si une date est avant ou après une autre. Vous pouvez également utiliser les fonctions `Add` et `Sub` pour ajouter ou soustraire une durée à une date.

De plus, Go prend en charge l'utilisation de formats de date et d'heure personnalisés en utilisant le package `time.Format`. Cela vous permet de spécifier exactement comment vous souhaitez afficher une date ou une heure dans votre code.

## Voir aussi

- Documentation officielle de Go sur le package `time` : https://golang.org/pkg/time/
- Tutoriel sur la manipulation des dates en Go : https://www.digitalocean.com/community/tutorials/how-to-manipulate-time-in-go-fr