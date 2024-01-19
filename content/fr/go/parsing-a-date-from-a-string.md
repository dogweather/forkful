---
title:                "Analyser une date à partir d'une chaîne"
html_title:           "Clojure: Analyser une date à partir d'une chaîne"
simple_title:         "Analyser une date à partir d'une chaîne"
programming_language: "Go"
category:             "Go"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/go/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

# Analyser une Date à Partir d'une Chaîne de Caractères en Go

## Quoi et Pourquoi ?

Analyser une date à partir d'une chaîne est le processus de conversion d'une chaîne de date en un format de date manipulable. Les programmeurs le font pour permettre des opérations plus complexes sur les dates, comme le calcul d'intervalles ou la comparaison de dates.

## Comment Faire :

Conversion d'une chaîne en date en utilisant le package "time".

```Go
package main

import (
	"time"
	"fmt"
)

func main() {
	str := "2022-04-10"
	format := "2006-01-02"

	t, _ := time.Parse(format, str)

	fmt.Println(t)
}
```

Ceci renvoie la date "2022-04-10 00:00:00 +0000 UTC".

## Approfondissement 

En matière de contexte historique, le package "time" a été introduit pour répondre aux besoins de manipulation de dates et d'heures en Go. Quand aux alternatives, vous pouvez utiliser des bibliothèques tierces comme "dateparse" qui peut analyser sans connaître le format.

Détail d'implémentation important : n'oubliez pas de gérer les erreurs dans le monde réel. Nous avons ignoré l'erreur dans notre exemple pour simplifier, mais dans une application réelle, vous voudrez gérer cette situation.

## Voir Aussi

- Go Time Package Docs: https://pkg.go.dev/time
- Bibliothèque dateparse: https://github.com/araddon/dateparse
- Go par exemple: https://gobyexample.com/time