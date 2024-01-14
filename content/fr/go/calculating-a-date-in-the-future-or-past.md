---
title:    "Go: Calculer une date dans le futur ou le passé"
keywords: ["Go"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fr/go/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Pourquoi

Calculer une date dans le futur ou dans le passé peut sembler être une tâche simple, mais en réalité, cela peut être un défi pour les programmeurs en Go. Il est important de comprendre la logique et les différentes méthodes pour le faire de manière efficace et précise. Dans cet article, nous allons explorer pourquoi et comment calculer une date en utilisant Go.

## Comment faire

Pour calculer une date dans le futur ou dans le passé en utilisant Go, nous allons utiliser la fonction `AddDate()` de la librairie `time`. Tout d'abord, nous devons déterminer la date de référence à partir de laquelle nous allons calculer. Ensuite, nous pouvons utiliser la fonction `AddDate()` en spécifiant le nombre d'années, de mois et de jours à ajouter ou soustraire à la date de référence.

Voici un exemple de code pour calculer une date dans le futur en ajoutant 1 an, 2 mois et 15 jours à la date actuelle :

```
Go package main

import (
	"fmt"
	"time"
)

func main() {
	// Définit la date de référence
	date := time.Now()

	// Ajoute 1 an, 2 mois et 15 jours à la date de référence
	futureDate := date.AddDate(1, 2, 15)

	fmt.Println("La date actuelle est:", date)
	fmt.Println("La date dans le futur est:", futureDate)
}
```

Cela devrait produire la sortie suivante :

```
La date actuelle est: 2021-02-20 12:00:00.0000
La date dans le futur est: 2022-04-05 12:00:00.0000
```

De manière similaire, nous pouvons calculer une date dans le passé en spécifiant un nombre négatif pour les années, les mois et les jours que nous voulons soustraire.

## Plongée en profondeur

Maintenant que nous avons vu comment calculer une date dans le futur ou dans le passé en utilisant Go, nous pouvons explorer plus en détails certaines des fonctionnalités et des options disponibles pour cette tâche.

Tout d'abord, il est important de noter que la fonction `AddDate()` renvoie une nouvelle instance de `time.Time` et ne modifie pas la date de référence d'origine.

De plus, la fonction `AddDate()` peut également être utilisée pour calculer des périodes de temps plus longues, comme des années bissextiles ou des siècles, en spécifiant le nombre approprié dans les paramètres. Par exemple, pour ajouter 10 siècles à la date de référence, nous pouvons utiliser : `AddDate(100, 0, 0)`.

Il est également possible de créer une nouvelle instance de `time.Time` en utilisant la fonction `Date()` et de spécifier directement l'année, le mois et le jour. Cela peut être utile si nous voulons calculer une date spécifique dans le futur ou dans le passé sans avoir à utiliser la fonction `AddDate()`.

## Voir aussi

Pour en savoir plus sur les fonctions disponibles pour le calcul des dates en utilisant Go, vous pouvez consulter la documentation officielle sur la librairie `time` : https://pkg.go.dev/time.

Vous pouvez également trouver d'autres astuces et exemples pratiques sur le site Go by Example : https://gobyexample.com/dates.