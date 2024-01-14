---
title:                "Go: Calcul d'une date dans le futur ou le passé"
simple_title:         "Calcul d'une date dans le futur ou le passé"
programming_language: "Go"
category:             "Go"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/go/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Pourquoi

Calculer une date dans le futur ou le passé peut être utile dans de nombreuses situations, comme la planification de projets, la gestion de tâches ou la création de rappels. De plus, cela peut être un bon moyen d'apprendre à utiliser des fonctions de base du langage de programmation Go.

## Comment Faire

Voici une façon simple de calculer une date dans le futur ou le passé en utilisant Go :

```Go
package main

import (
    "fmt"
    "time"
)

func main() {

    // Définir une date de référence
    currentDate := time.Now()
    
    // Ajouter ou soustraire un nombre de jours à la date de référence
    futureDate := currentDate.AddDate(0, 0, 7) // ajoute 7 jours
    pastDate := currentDate.AddDate(0, 0, -7)   // soustrait 7 jours
    
    // Afficher les dates calculées 
    fmt.Println("Date actuelle:", currentDate.Format("02/01/2006"))
    fmt.Println("Date dans 7 jours:", futureDate.Format("02/01/2006"))
    fmt.Println("Date il y a 7 jours:", pastDate.Format("02/01/2006"))
}
```

Voici un exemple de sortie pour cette fonction :

```
Date actuelle: 23/05/2020
Date dans 7 jours: 30/05/2020
Date il y a 7 jours: 16/05/2020
```

## Approfondissement

En plus de la méthode `AddDate` utilisée dans l'exemple précédent, Go propose d'autres fonctions utiles pour calculer des dates dans le futur ou le passé. Par exemple, `Add` permet d'ajouter ou de soustraire un nombre spécifique de secondes, minutes, heures ou années à une date de référence. Il est également possible d'utiliser `Date` pour créer une nouvelle date en spécifiant une année, un mois et un jour spécifiques. Pour plus d'informations sur ces fonctions et d'autres outils de manipulation de dates en Go, consultez la [documentation officielle](https://golang.org/pkg/time/).

## Voir Aussi

- [Documentation officielle sur la manipulation de dates en Go](https://golang.org/pkg/time/)
- [Article sur la manipulation de dates en Go](https://www.digitalocean.com/community/tutorials/how-to-manipulate-date-and-time-in-go)
- [Exemples de calcul de dates en Go](https://play.golang.org/p/2udnDtwq6Ky)