---
title:    "Go: Calculer une date dans le futur ou le passé"
keywords: ["Go"]
---

{{< edit_this_page >}}

# Pourquoi
Pourquoi quelqu'un voudrait-il calculer une date dans le futur ou le passé? Parfois, cela peut être utile pour planifier des événements ou pour vérifier des échéances. Par exemple, un programmeur peut avoir besoin de vérifier si une certaine date est un jour férié ou un week-end pour planifier des déploiements de logiciels.

# Comment faire
Il existe plusieurs façons de calculer une date dans le futur ou le passé en utilisant le langage Go. Tout d'abord, nous devons importer le package `time` qui contient les fonctions nécessaires pour manipuler les dates. Ensuite, nous pouvons utiliser la fonction `time.Now()` pour obtenir la date actuelle. Voici un exemple de code pour calculer la date dans 7 jours à partir de maintenant:

```Go
package main
import (
	"fmt"
	"time"
)
func main() {
	now := time.Now()
	fmt.Printf("La date actuelle est: %v\n", now)
	future := now.AddDate(0, 0, 7)
	fmt.Printf("La date dans 7 jours sera: %v\n", future)
}
```

La sortie de ce code serait:

```
La date actuelle est: 2021-07-01 12:00:00 +0000 UTC m=+0.000000001
La date dans 7 jours sera: 2021-07-08 12:00:00 +0000 UTC m=+604800.000000001
```

# Plongée en profondeur
La fonction `time.AddDate()` que nous avons utilisée dans l'exemple précédent prend trois arguments: le nombre d'années, de mois et de jours à ajouter à la date actuelle. Si nous voulons calculer une date dans le passé, nous pouvons utiliser des valeurs négatives pour ces arguments. Par exemple, si nous voulons calculer la date d'il y a deux mois, nous pouvons utiliser `now.AddDate(0, -2, 0)`.

De plus, le package `time` offre d'autres fonctions pour manipuler les dates, telles que `time.Parse()` pour convertir une chaîne de caractères en type `time.Time` et `time.UTC()` pour convertir une date en temps universel coordonné. Vous pouvez en apprendre plus sur ces fonctions en consultant la documentation officielle du package.

# Voir aussi
- [Documentation officielle de time package](https://pkg.go.dev/time)
- [Tutoriel sur les dates et les heures en Go](https://golangdocs.com/golang-datetime)
- [Exemples de manipulation de dates en Go](https://zetcode.com/golang/date-time/)