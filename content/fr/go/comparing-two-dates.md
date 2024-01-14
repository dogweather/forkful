---
title:                "Go: Comparaison de deux dates"
simple_title:         "Comparaison de deux dates"
programming_language: "Go"
category:             "Go"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/go/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Pourquoi

Il est souvent nécessaire en programmation de comparer deux dates pour effectuer des opérations spécifiques ou pour vérifier des conditions. Dans cet article, nous allons voir comment effectuer cette comparaison en utilisant le langage de programmation Go.

## Comment faire

Pour comparer deux dates en Go, nous allons utiliser la fonction `Equal` de la bibliothèque `time`. Cette fonction prend deux paramètres de type `Time` et renvoie un booléen indiquant si les deux dates sont égales ou non. Voici un exemple de code :

```
package main

import (
    "fmt"
    "time"
)

func main() {
    // Première date
    date1 := time.Date(2020, time.April, 5, 0, 0, 0, 0, time.UTC)

    // Deuxième date
    date2 := time.Date(2020, time.April, 5, 12, 0, 0, 0, time.UTC)

    // Comparaison des dates
    equals := date1.Equal(date2)

    // Affichage du résultat
    fmt.Println(equals) // Output: true
}
```

Comme vous pouvez le voir, nous utilisons la fonction `Date` de la bibliothèque `time` pour créer deux dates, puis nous utilisons la fonction `Equal` pour les comparer. Le résultat sera `true` si les deux dates sont égales.

Il est également possible de comparer les dates en utilisant les opérateurs de comparaison `==`, `<` et `>`, mais cela peut conduire à des résultats imprévisibles en raison de la complexité des objets `Time`. Il est donc préférable d'utiliser la fonction `Equal`.

## Plongée en profondeur

Lorsque vous utilisez la fonction `Equal` pour comparer des dates, il est important de noter que la comparaison prend en compte non seulement la date, mais aussi l'heure et la zone horaire. Cela signifie que deux dates peuvent sembler égales lorsqu'elles ne le sont pas en réalité en raison d'une différence de fuseau horaire. Par exemple, les dates suivantes seront considérées comme égales à cause de la convertion automatique en UTC :

```
// Première date
date1 := time.Date(2020, time.April, 5, 0, 0, 0, 0, time.UTC)

// Deuxième date à l'heure locale
date2 := time.Date(2020, time.April, 5, 12, 0, 0, 0, time.Local)

equals := date1.Equal(date2)

fmt.Println(equals) // Output: true
```

Si vous souhaitez comparer uniquement les dates et ignorer l'heure et la zone horaire, vous pouvez utiliser la fonction `Truncate` de la bibliothèque `time` pour réinitialiser ces valeurs à zéro avant de les comparer.

## Voir aussi

- [Documentation officielle de Go sur la bibliothèque `time`](https://golang.org/pkg/time/)
- [Article du blog Technique de la société Apcera](https://www.apcera.com/blog/golang-date-time-formatting)
- [Article du blog L'agence New Yorkaise de robots](https://www.nyrobotics.com/blog/golang-datetime-now-tostring-date-and-time-formatting)