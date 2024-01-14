---
title:                "Go: Comparer deux dates"
programming_language: "Go"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/go/comparing-two-dates.md"
---

{{< edit_this_page >}}

# Pourquoi

La comparaison de deux dates est une tâche courante en programmation, en particulier dans le développement d'applications web ou de systèmes de gestion de bases de données. Cela permet de vérifier l'ordre chronologique des événements ou de déterminer si une date se situe avant ou après une autre.

# Comment faire

Voici un exemple simple en Go pour comparer deux dates :

```Go
package main

import (
	"fmt"
	"time"
)

func main() {
	// Définition des deux dates à comparer
	date1 := time.Date(2021, time.January, 1, 0, 0, 0, 0, time.UTC)
	date2 := time.Date(2020, time.December, 1, 0, 0, 0, 0, time.UTC)

	// Comparaison avec la méthode Equal
	if date1.Equal(date2) {
		fmt.Println("Les deux dates sont égales.")
	} else {
		fmt.Println("Les deux dates sont différentes.")
	}

	// Comparaison avec la méthode After
	if date1.After(date2) {
		fmt.Println("La date 1 est après la date 2.")
	} else {
		fmt.Println("La date 1 est avant la date 2.")
	}

	// Comparaison avec la méthode Before
	if date1.Before(date2) {
		fmt.Println("La date 1 est avant la date 2.")
	} else {
		fmt.Println("La date 1 est après la date 2.")
	}
}
```

La sortie de ce code sera :

```
Les deux dates sont différentes.
La date 1 est après la date 2.
La date 1 est avant la date 2.
```

Il est important de noter que les dates doivent être dans le même fuseau horaire pour être correctement comparées.

# Deep Dive

Les méthodes Equal, After et Before utilisées dans l'exemple ci-dessus sont des fonctions de la bibliothèque standard de Go pour comparer les dates. Elles retournent toutes un booléen en fonction du résultat de la comparaison.

Il existe également d'autres méthodes pour comparer des dates plus en profondeur, telles que la méthode EqualFold qui ignore les différences de casse entre les dates ou la méthode Since qui calcule la différence entre les deux dates en termes de durée.

Il est également possible de comparer des dates avec la méthode EqualDate de la bibliothèque "golang.org/x/tools/imports", qui permet de comparer les dates sans tenir compte du fuseau horaire.

# Voir aussi

- https://golang.org/pkg/time/
- https://yourbasic.org/golang/compare-time/
- https://godoc.org/golang.org/x/tools/imports#EqualDate (comparaison de dates avec EqualDate)