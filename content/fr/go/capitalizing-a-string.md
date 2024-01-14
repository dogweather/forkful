---
title:                "Go: Majuscule d'une chaîne de caractères"
programming_language: "Go"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/go/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Pourquoi

Il existe de nombreuses raisons pour lesquelles vous pourriez avoir besoin de capitaliser une chaîne de caractères dans votre code Go. Peut-être que vous souhaitez formater correctement les noms propres, ou peut-être que vous devez respecter des conventions de style spécifiques dans votre projet. Quelle que soit la raison, il est important de savoir comment capitaliser une chaîne en utilisant Go.

## Comment faire

Heureusement, Go a une fonction intégrée appelée "strings.Title" qui peut être utilisée pour capitaliser une chaîne donnée. Voici un exemple de code montrant comment utiliser cette fonction :

```Go
package main

import "fmt"
import "strings"

func main() {
    myString := "bonjour tout le monde"
    fmt.Println(strings.Title(myString))
}
```

Lorsque vous exécutez ce code, vous verrez que la chaîne de caractères "bonjour tout le monde" est maintenant capitalisée en "Bonjour Tout Le Monde". Vous pouvez également utiliser cette fonction pour capitaliser une chaîne stockée dans une variable, plutôt que de fournir une chaîne directement dans la fonction "strings.Title".

## Plongée profonde

Bien que la fonction "strings.Title" soit utile pour la plupart des cas de capitalisation, il y a quelques exceptions à garder à l'esprit. Par exemple, elle ne capitalisera pas les lettres après les apostrophes ou les traits d'union. Si vous avez besoin d'une capitalisation plus précise, vous devrez peut-être écrire votre propre fonction ou utiliser une bibliothèque tierce.

De plus, si vous travaillez avec des chaînes en unicode, la fonction "strings.Title" peut ne pas fonctionner correctement. Dans ce cas, vous devrez peut-être utiliser la bibliothèque "golang.org/x/text/unicode/norm" pour normaliser la chaîne avant de la capitaliser.

## Voir aussi

- [Documentation officielle de Go sur la fonction strings.Title](https://golang.org/pkg/strings/#Title)
- [Exemples de capitalisation en utilisant Go](https://gobyexample.com/string-functions)
- [Bibliothèque Go pour la normalisation de chaînes unicode](https://godoc.org/golang.org/x/text/unicode/norm)