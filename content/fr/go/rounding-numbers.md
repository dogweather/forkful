---
title:                "Arrondir les nombres"
date:                  2024-01-26T03:44:38.380010-07:00
model:                 gpt-4-0125-preview
simple_title:         "Arrondir les nombres"

tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/go/rounding-numbers.md"
---

{{< edit_this_page >}}

## Quoi & Pourquoi ?
Arrondir des nombres signifie ajuster un nombre à son entier le plus proche ou à une décimale spécifiée. Cela se fait pour simplifier les valeurs, les rendre plus lisibles, ou les faire correspondre à certaines contraintes, comme lors du travail avec des devises.

## Comment faire :
Le paquet `math` de Go est votre ami pour l'arrondissement. Utilisez `math.Round`, `math.Floor`, et `math.Ceil` pour simplifier :

```go
package main

import (
	"fmt"
	"math"
)

func main() {
	number := 3.14159
	fmt.Println("Round:", math.Round(number))  // Arrondir au nombre entier le plus proche
	fmt.Println("Floor:", math.Floor(number)) // Arrondir à l'inférieur
	fmt.Println("Ceil: ", math.Ceil(number))  // Arrondir à supérieur
}
```

Sortie d'exemple :
```
Round: 3
Floor: 3
Ceil: 4
```

Pour des places décimales spécifiques, multipliez, arrondissez, puis divisez :

```go
func roundToDecimalPlace(number float64, decimalPlaces int) float64 {
	shift := math.Pow(10, float64(decimalPlaces))
	return math.Round(number*shift) / shift
}

func main() {
	number := 3.14159
	fmt.Println("Arrondi à 2 décimales :", roundToDecimalPlace(number, 2))
}
```

Sortie d'exemple :
```
Arrondi à 2 décimales : 3.14
```

## Approfondissement
Arrondir les nombres n'est pas nouveau - cela remonte aux mathématiques anciennes, visant toujours la simplicité. Le `math.Round` dans Go utilise [l'arrondissement des banquiers](https://fr.wikipedia.org/wiki/Arrondi#Arrondi_à_l'entier_le_plus_proche), signifiant que 0,5 est arrondi au nombre pair le plus proche, réduisant un biais qui pourrait affecter les sommes.

Les nombres à virgule flottante peuvent être délicats en raison de leur représentation binaire, qui pourrait ne pas représenter exactement tous les décimaux. Cependant, l'approche de Go maintient le comportement attendu la plupart du temps.

D'autres méthodes d'arrondissement existent, comme "arrondir au demi-supérieur" ou "arrondir au demi-éloigné de zéro", mais la bibliothèque standard de Go est ce qui est immédiatement disponible. Pour des besoins plus complexes, vous pourriez avoir besoin d'une bibliothèque tierce ou développer votre propre solution.

## Voir aussi
- Le paquet `math` de Go : [https://pkg.go.dev/math](https://pkg.go.dev/math)
- La norme IEEE 754 pour l'arithmétique à virgule flottante (base de Go pour la gestion des flottants) : [https://ieeexplore.ieee.org/document/4610935](https://ieeexplore.ieee.org/document/4610935)
- Comprendre la virgule flottante : ["Ce que chaque informaticien devrait savoir sur l'arithmétique à virgule flottante"](https://docs.oracle.com/cd/E19957-01/806-3568/ncg_goldberg.html)
