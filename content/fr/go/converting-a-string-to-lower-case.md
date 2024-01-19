---
title:                "Convertir une chaîne en minuscules"
html_title:           "Arduino: Convertir une chaîne en minuscules"
simple_title:         "Convertir une chaîne en minuscules"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/go/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Quoi & Pourquoi ?
La conversion d'une chaîne en minuscules dans la programmation est le processus consistant à changer tous les caractères alphabétiques en lettres minuscules. Les programmeurs le font souvent pour normaliser ou homogénéiser les données afin d'optimiser la correspondance et la recherche des chaînes.

## Comment faire :
En Go, nous utilisons simplement la méthode `ToLower` du package `strings`. Voici un exemple d'utilisation :

```Go
package main

import (
    "fmt"
    "strings"
)

func main() {
    str := "Hello, World!"
    lower := strings.ToLower(str)
    fmt.Println(lower)
}
```

Affiche:

```Go
hello, world!
```

## Précisions :
En termes historiques, la conversion en minuscules est une pratique courante depuis les premiers jours de l'informatique. En Go, la fonction `ToLower` utilise une carte interne pour correspondre aux lettres majuscules et les convertir en minuscules. 

Côté alternatives, on peut écrire une fonction personnalisée qui utilise la table ASCII pour convertir une chaîne en minuscules, cependant, cette approche ne gère pas les caractères non ASCII. Pour cette raison, et pour une question de simplicité, les programmeurs préfèrent utiliser la méthode `ToLower`.

## Voir également :
- Documentation officielle Go pour la méthode strings.ToLower: https://golang.org/pkg/strings/#ToLower
- Tutoriel Go sur le package strings : https://www.golangprograms.com/go-language-package-functions-and-methods.html
- Cours Go sur le traitement des chaînes : http://www.golang-book.com/books/intro/12