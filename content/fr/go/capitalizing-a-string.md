---
title:    "Go: Majusculation d'une chaîne de caractères"
keywords: ["Go"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fr/go/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Pourquoi

Dans la programmation Go, il peut être utile de convertir une chaîne de caractères en majuscules pour des raisons telles que le tri ou l'affichage de données. Dans cet article, nous allons explorer comment capitaliser une chaîne de caractères en utilisant Go.

## Comment Faire

Nous pouvons utiliser la fonction `strings.ToUpper()` pour convertir une chaîne de caractères en majuscules. Voici un exemple de code:

```Go
package main

import (
	"fmt"
	"strings"
)

func main() {
	str := "bonjour le monde"
	capitalized := strings.ToUpper(str)
	fmt.Println(capitalized)
}
```

Lorsque nous exécutons ce code, nous obtenons la sortie suivante:

```
BONJOUR LE MONDE
```

Nous pouvons également utiliser la fonction `strings.Title()` pour capitaliser la première lettre de chaque mot dans la chaîne. Voici un exemple de code:

```Go
package main

import (
	"fmt"
	"strings"
)

func main() {
	str := "bonjour le monde"
	capitalized := strings.Title(str)
	fmt.Println(capitalized)
}
```

La sortie serait:

```
Bonjour Le Monde
```

## Plongée en Profondeur

Il est important de noter que ces fonctions ne modifient pas la chaîne originale, mais renvoient une nouvelle chaîne de caractères avec les modifications. Nous pouvons également utiliser la fonction `strings.ToLower()` pour convertir une chaîne en minuscules. Enfin, nous pouvons utiliser la fonction `strings.ToUpperSpecial()` pour spécifier une langue spécifique pour la conversion.

## Voir Aussi

- La documentation officielle sur les fonctions de manipulation de chaînes de caractères de Go: https://pkg.go.dev/strings
- Un tutoriel vidéo sur les chaînes de caractères en Go: https://www.youtube.com/watch?v=A_1bs64r3h4
- Un exemple de projet utilisant la manipulation de chaînes de caractères en Go: https://github.com/golang/tour/tree/master/stringutil