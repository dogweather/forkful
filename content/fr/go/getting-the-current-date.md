---
title:                "Obtenir la date actuelle"
date:                  2024-01-20T15:14:28.489671-07:00
html_title:           "C: Obtenir la date actuelle"
simple_title:         "Obtenir la date actuelle"

category:             "Go"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/go/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Quoi & Pourquoi ?
Obtenir la date actuelle en programmation, c'est comme demander à votre montre quelle heure il est. Les développeurs en ont besoin pour tout, que ce soit pour horodater des événements, planifier des tâches ou simplement afficher la date d'aujourd'hui à l'utilisateur.

## Comment faire :
```Go
package main

import (
	"fmt"
	"time"
)

func main() {
	// Obtenir la date actuelle
	currentTime := time.Now()
	
	// Afficher la date actuelle au format standard
	fmt.Println("Date actuelle :", currentTime.Format("2006-01-02 15:04:05"))
	
	// Juste la date sans l'heure
	fmt.Println("Juste la date :", currentTime.Format("2006-01-02"))
}
```

Sortie :
```
Date actuelle : 2023-04-05 14:07:31
Juste la date : 2023-04-05
```

## Plongée en profondeur
Historiquement, obtenir la date et l'heure était une affaire de système d'exploitation. En Go, le package `time` simplifie cette tâche, gérant les complexités des zones horaires et de la localisation. Assigner `time.Now()` à une variable nous donne un objet `Time` contenant la date et l'heure actuelle. Go utilise un formatage date/heure basé sur un moment précis : `Mon Jan 2 15:04:05 MST 2006`. C'est amusant, c'est comme les développeurs Go utilisaient une phrase mnémonique pour se souvenir du format. En alternative, on peut aussi utiliser des bibliothèques tierces pour plus de fonctionnalités, mais `time` suffit en général.

## Voir également
- [Documentation du package `time`](https://pkg.go.dev/time)
- [Formatage et parsing de dates en Go](https://yourbasic.org/golang/format-parse-string-time-date-example/)
- [Gestion des fuseaux horaires avec Go](https://medium.com/@dhanushgopinath/working-with-time-packages-and-timezones-in-go-9a4b1ae3a8a2)
