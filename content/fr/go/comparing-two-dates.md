---
title:                "Comparer deux dates"
date:                  2024-01-20T17:33:10.462716-07:00
model:                 gpt-4-1106-preview
simple_title:         "Comparer deux dates"
programming_language: "Go"
category:             "Go"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/go/comparing-two-dates.md"
---

{{< edit_this_page >}}

## What & Why?
Comparer deux dates, c'est comme mesurer la différence entre deux moments dans le temps. Les programmeurs le font pour planifier des événements, vérifier des délais ou trier des historiques.

## How to:
```Go
package main

import (
	"fmt"
	"time"
)

func main() {
	// Définir deux dates
	date1 := time.Date(2023, 04, 01, 10, 0, 0, 0, time.UTC)
	date2 := time.Date(2023, 04, 12, 10, 0, 0, 0, time.UTC)

	// Comparer les dates
	diff := date2.Sub(date1)

	// Afficher le résultat
	fmt.Printf("La différence entre les dates est de: %v\n", diff)

	// Vérifier si une date est avant ou après l'autre
	if date1.Before(date2) {
		fmt.Println("La première date est avant la seconde.")
	} else {
		fmt.Println("La première date est après la seconde.")
	}
}
```
Résultat:
```
La différence entre les dates est de: 264h0m0s
La première date est avant la seconde.
```

## Deep Dive
La comparaison de dates en Go utilise le package `time`. Avant 2006, les développeurs comptaient souvent sur des librairies tierces pour gérer les dates et leur comparaison. Les fonctions `Before`, `After` et `Equal` vérifient les relations temporelles. `Sub` donne la durée entre deux instants. Go gère le temps avec une précision nanoseconde, ce qui le rend très précis pour des applications critiques comme les bases de données ou les systèmes de commande.

## See Also
- Documentation officielle de Go pour le package `time`: [Package time](https://pkg.go.dev/time)