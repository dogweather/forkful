---
title:                "Go: Translation: Conversion d'une chaîne de caractères en minuscules"
programming_language: "Go"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/go/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

# Pourquoi convertir une chaîne en minuscules en Go?

La conversion d'une chaîne en minuscules peut être utile pour la normalisation des données, la comparaison de chaînes sans tenir compte de la casse ou pour améliorer les performances lors de l'exécution de certaines opérations sur de grandes quantités de données.

## Comment le faire en Go

La fonction `strings.ToLower()` peut être utilisée pour convertir une chaîne en minuscules en Go. Voici un exemple de code pour illustrer cela:

```Go
package main

import (
	"fmt"
	"strings"
)

func main() {
	str := "JE SUIS UNE CHAÎNE EN MAJUSCULES"
	lower := strings.ToLower(str)
	fmt.Println(lower)
}
```

Ceci affichera la sortie suivante: `je suis une chaîne en majuscules`.

## Plongée en profondeur

En Go, les chaînes sont des tableaux de bytes, ce qui signifie qu'elles peuvent être facilement modifiées en convertissant les lettres en bytes correspondants et en soustrayant 32 pour les lettres majuscules afin d'obtenir les lettres correspondantes en minuscules. Cependant, l'utilisation de la fonction prédéfinie `strings.ToLower()` est généralement préférable pour des raisons de lisibilité, de maintenabilité et de performances.

# Voir aussi

- Documentation officielle de Go sur les chaînes: https://golang.org/ref/spec#String_types
- Tutoriel sur les chaînes en Go: https://gobyexample.com/strings
- Comparaison de chaînes en Go: https://www.golangprograms.com/golang-compare-two-strings.html