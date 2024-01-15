---
title:                "Convertir une chaîne en minuscules"
html_title:           "Go: Convertir une chaîne en minuscules"
simple_title:         "Convertir une chaîne en minuscules"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/go/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Pourquoi

Si vous travaillez avec des chaînes de caractères en Go, il peut être utile de les convertir en lettres minuscules pour une meilleure manipulation et comparaison de données. Cela peut également être utile lors de la validation des entrées utilisateur pour garantir une cohérence dans le traitement de ces données.

## Comment faire

Pour convertir une chaîne de caractères en lettres minuscules en utilisant Go, vous pouvez utiliser la fonction `ToLower()` de la bibliothèque standard `strings`. Voici un exemple d'utilisation :

```Go
package main

import (
	"fmt"
	"strings"
)

func main() {
	string1 := "Go Programming"
	fmt.Println(strings.ToLower(string1))
}
```

La sortie de ce code sera "go programming". La fonction `ToLower()` renvoie une nouvelle chaîne de caractères en lettres minuscules sans modifier la chaîne d'origine.

## Plongée en profondeur

La fonction `ToLower()` utilise le jeu de caractères Unicode pour convertir les lettres en minuscules. Cela signifie qu'elle prend en charge les alphabets non latins ainsi que les caractères accentués. Cela peut être utile lors du traitement de données multilingues.

De plus, il existe d'autres fonctions comme `ToUpper()` et `Title()` qui peuvent être utilisées pour convertir une chaîne en lettres majuscules ou pour mettre en majuscule la première lettre de chaque mot respectivement.

## Voir aussi

- Documentation officielle de Go sur les chaînes de caractères : https://golang.org/pkg/strings/
- Tutoriel vidéo sur la manipulation de chaînes de caractères en Go : https://youtu.be/lgwOeH53fvg
- Exemples de code sur la conversion de chaînes en lettres minuscules : https://github.com/avelino/awesome-go#string