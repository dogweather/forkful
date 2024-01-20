---
title:                "Analyse d'une date à partir d'une chaîne de caractères"
date:                  2024-01-20T15:36:17.553501-07:00
html_title:           "Arduino: Analyse d'une date à partir d'une chaîne de caractères"
simple_title:         "Analyse d'une date à partir d'une chaîne de caractères"
programming_language: "Go"
category:             "Go"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/go/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## What & Why?
Parse un date, ça veut dire convertir une chaîne de caractères en une structure de date que le programme peut comprendre et manipuler. Les programmeurs le font pour traiter des dates (log, données utilisateur, etc.) en formats variés.

## How to:
```Go
package main

import (
	"fmt"
	"time"
)

func main() {
	// Exemple de chaîne de caractères avec une date
	dateStr := "02/01/2006 15:04:05"

	// Conversion de la chaîne de caractères en date
	parsedDate, err := time.Parse("02/01/2006 15:04:05", dateStr)
	if err != nil {
		panic(err)
	}

	// Affichage de la date parsée
	fmt.Println(parsedDate)
}
```

Sortie attendue (dépend de votre timezone):
```
2006-01-02 15:04:05 +0000 UTC
```

## Deep Dive
Historiquement, la manipulation des dates a toujours été critique en programmation. Des formats standards, comme ISO 8601, ont été créés pour homogénéiser les représentations des dates et heures. En Go, la bibliothèque `time` est l'outil préféré pour ça, utilisant un modèle unique pour interpréter les dates. Parmi les alternatives, on trouve `ParseInLocation` pour gérer des fuseaux horaires spécifiques, ou des paquets tiers comme `dateparse`.

L'implémentation en Go utilise des modèles de date et heure prédéfinis dans le package `time`, par exemple `time.RFC3339` pour le format RFC 3339. Le design est apprécié pour sa simplicité, bien qu'il puisse être déroutant au début car il requiert l'utilisation d'une date spécifique (`Mon Jan 2 15:04:05 MST 2006`) comme référence.

## See Also
- Documentation officielle de Go pour le package `time`: [https://pkg.go.dev/time](https://pkg.go.dev/time)
- Un aperçu sur les formats de date et heure en Go: [https://yourbasic.org/golang/format-parse-string-time-date-example/](https://yourbasic.org/golang/format-parse-string-time-date-example/)