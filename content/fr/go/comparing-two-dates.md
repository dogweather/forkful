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

## Quoi & Pourquoi?
Comparer deux dates en programmation signifie comparer les valeurs de dates pour déterminer si elles sont égales, avant ou après l'autre, ou si elles sont dans un intervalle spécifique. Les programmeurs le font souvent pour trier et organiser des données chronologiquement ou pour suivre les changements dans le temps.

## Comment faire:
Voici un exemple de code pour comparer deux dates en utilisant la syntaxe de Go:
```Go
date1 := time.Date(2020, 1, 1, 0, 0, 0, 0, time.UTC)
date2 := time.Date(2021, 1, 1, 0, 0, 0, 0, time.UTC)

if date1.Before(date2) {
  fmt.Println("Date 1 est avant Date 2")
} else if date1.After(date2) {
  fmt.Println("Date 1 est après Date 2")
} else if date1.Equal(date2) {
  fmt.Println("Date 1 et Date 2 sont égales")
}
```
Résultat: Date 1 est avant Date 2

## Plongée en profondeur:
L'idée de comparer des dates vient de la nécessité de stocker et d'organiser des données dans un ordre chronologique. Les programmeurs peuvent également utiliser des alternatives telles que le format numérique de temps, qui utilise des nombres pour représenter une date et une heure spécifiques. En termes d'implémentation, Go utilise la méthode "Before", "After" et "Equal" pour comparer des dates en utilisant le type de données "time".

## À voir aussi:
- [Documentation officielle de Go sur la comparaison de dates](https://pkg.go.dev/time?tab=doc#example-Time-Before)
- [Article de blog sur la comparaison de dates en Go](https://www.calhoun.io/comparing-times-dates-in-go/)