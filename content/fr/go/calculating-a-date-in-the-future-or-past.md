---
title:                "Calcul d'une date future ou passée"
date:                  2024-01-20T17:31:22.067340-07:00
model:                 gpt-4-1106-preview
simple_title:         "Calcul d'une date future ou passée"
programming_language: "Go"
category:             "Go"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/go/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## What & Why?
Calculer une date dans le futur ou le passé, c’est déterminer un jour spécifique avant ou après une date donnée. C’est crucial pour gérer des échéances, des abonnements et tout ce qui touche à la planification.

## How to:
En Go, on manipule les dates avec le package `time`. Voici un petit tuto pour ajouter et soustraire des jours.

```Go
package main

import (
	"fmt"
	"time"
)

func main() {
	// Date actuelle
	today := time.Now()
	fmt.Println("Aujourd'hui :", today.Format("02-01-2006"))

	// Ajout de 10 jours
	future := today.AddDate(0, 0, 10)
	fmt.Println("Dans 10 jours :", future.Format("02-01-2006"))

	// Soustraction de 10 jours
	past := today.AddDate(0, 0, -10)
	fmt.Println("Il y a 10 jours :", past.Format("02-01-2006"))
}
```
Sortie:
```
Aujourd'hui : 30-03-2023
Dans 10 jours : 09-04-2023
Il y a 10 jours : 20-03-2023
```

## Deep Dive
Avant Go, les langages comme C avaient la librairie `<time.h>` pour manipuler le temps. En Go, tout est plus simple et robuste avec `time`. Pourquoi ? Parce que `time.Time` gère le temps avec précision nanoseconde et les zones horaires dès le départ. 

Les alternatives peuvent inclure l’utilisation de bibliothèques tierces, mais souvent, le standard de Go suffit. Si vous devez gérer des formats de date compliqués, regardez du côté de `github.com/araddon/dateparse` pour une flexibilité accrue.

Les détails d'implémentation ? Les calculs sur les dates utilisent `time.Add` pour des durées précises ou `time.AddDate` pour ajouter ou retirer des années, mois et jours. C'est sûr et respecte les anomalies comme les années bissextiles.

## See Also
- Documentation officielle `time` package : https://golang.org/pkg/time/
- Article sur la gestion des dates et heures en Go : https://yourbasic.org/golang/time-change-date-and-time-format/
- dateparse pour plus de flexibilité : https://github.com/araddon/dateparse