---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 17:57:36.146722-07:00
description: "Obtenir la date actuelle en Go est une t\xE2che fondamentale pour les\
  \ programmeurs, au m\xEAme titre que \"Hello, World!\" en raison de son omnipr\xE9\
  sence. C'est\u2026"
lastmod: '2024-03-13T22:44:57.145724-06:00'
model: gpt-4-0125-preview
summary: "Obtenir la date actuelle en Go est une t\xE2che fondamentale pour les programmeurs,\
  \ au m\xEAme titre que \"Hello, World!\" en raison de son omnipr\xE9sence. C'est\u2026"
title: Obtenir la date actuelle
---

{{< edit_this_page >}}

## Quoi & Pourquoi ?

Obtenir la date actuelle en Go est une tâche fondamentale pour les programmeurs, au même titre que "Hello, World!" en raison de son omniprésence. C'est essentiel pour des tâches allant de l'enregistrement et l'horodatage des événements au calcul des durées et à la planification des événements futurs.

## Comment faire :

En Go, le package `time` est votre porte d'entrée pour travailler avec les dates et les heures. La fonction `time.Now()` vous donne la date et l'heure actuelles, tandis que d'autres fonctions et méthodes vous permettent de formater ou de manipuler ces données. Voici comment obtenir la date actuelle et ses diverses représentations :

```go
package main

import (
	"fmt"
	"time"
)

func main() {
	currentTime := time.Now() // Obtient la date et l'heure actuelles
	fmt.Println("Heure actuelle:", currentTime)

	// Pour obtenir la date au format AAAA-MM-JJ
	fmt.Println("Date actuelle:", currentTime.Format("2006-01-02"))

	// Pour obtenir les composantes individuelles de la date
	year, month, day := currentTime.Date()
	fmt.Printf("Année : %d, Mois : %s, Jour : %d\n", year, month, day)

	// Pour obtenir le jour de la semaine
	fmt.Println("Jour de la semaine:", currentTime.Weekday())
}
```

Un exemple de sortie pourrait ressembler à ceci :

```
Heure actuelle : 2023-04-18 15:04:05.123456 +0000 UTC
Date actuelle : 2023-04-18
Année : 2023, Mois : Avril, Jour : 18
Jour de la semaine : Mardi
```

Remarquez comment `Format` utilise une date spécifique (2006-01-02) comme chaîne de mise en forme. C'est la date de référence choisie par Go, servant de motif mnémotechnique pour le formatage des dates.

## Plongée en profondeur

La décision d'utiliser le package `time` pour la manipulation des dates et des heures en Go reflète le dévouement du langage à des bibliothèques standard robustes et intuitives. Contrairement à certains langages qui peuvent avoir plusieurs bibliothèques ou méthodologies concurrentes pour la manipulation des dates, Go privilégie l'existence d'une norme unique et bien documentée.

Le choix particulier de la date de référence (`Mon Jan 2 15:04:05 MST 2006`) dans le formatage du temps de Go, bien que initialement déroutant, est en réalité un coup de maître en termes d'usabilité. Il permet aux programmeurs de représenter les formats de date et d'heure en utilisant une approche basée sur des exemples, au lieu de devoir mémoriser des jetons ou des symboles que d'autres langages pourraient utiliser.

Cela dit, bien que le package `time` offre une fonctionnalité complète pour la plupart des besoins, le traitement des fuseaux horaires et des changements d'heure d'été (DST) peut parfois dérouter les nouveaux programmeurs en Go. Il est crucial de comprendre comment Go gère le temps spécifique à un lieu pour éviter les pièges courants dans la manipulation du temps.

Pour des besoins de planification ou de manipulation du temps plus complexes, des bibliothèques tierces telles que `github.com/robfig/cron` pour Go pourraient offrir une fonctionnalité plus spécialisée que le package `time` standard. Cependant, pour la plupart des applications nécessitant l'obtention et la gestion de la date et de l'heure actuelles, le package `time` offre un point de départ solide et idiomatique en Go.
