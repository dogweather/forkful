---
title:                "Go: Capitalisation d'une chaîne de caractères"
simple_title:         "Capitalisation d'une chaîne de caractères"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/go/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Pourquoi
Vous êtes-vous déjà demandé pourquoi il est important de capitaliser une chaîne de caractères en programmation ? Dans cet article, nous allons explorer cette question et vous montrer comment le faire en utilisant le langage de programmation Go.

## Comment faire
La méthode la plus courante pour capitaliser une chaîne de caractères en Go est d'utiliser la fonction `strings.ToUpper()`. Voici un exemple de code qui montre comment l'utiliser :

```Go
package main

import "fmt"
import "strings"

func main() {
    str := "bonjour"
    fmt.Println(strings.ToUpper(str))
}
```

Lorsque vous exécuterez ce code, vous obtiendrez l'output suivant :

```
BONJOUR
```

Comme vous pouvez le voir, la fonction `strings.ToUpper()` a transformé la chaîne de caractères "bonjour" en "BONJOUR". Cela vous permet de capitaliser facilement une chaîne de caractères en utilisant Go.

## Deep Dive
Maintenant que vous savez comment capitaliser une chaîne de caractères en Go, plongeons un peu plus profondément dans ce sujet. Il existe en fait deux façons de capitaliser une chaîne de caractères en Go : avec la fonction `strings.ToUpper()` que nous avons utilisée précédemment, ou en utilisant la fonction `strings.Title()`. La fonction `strings.Title()` va capitialiser la première lettre de chaque mot dans une chaîne de caractères, tandis que la fonction `strings.ToUpper()` va capitaliser toutes les lettres. Par exemple :

```Go
package main

import "fmt"
import "strings"

func main() {
    str := "bonjour tout le monde"
    fmt.Println(strings.Title(str))
    fmt.Println(strings.ToUpper(str))
}
```

L'output de ce code sera :

```
Bonjour Tout Le Monde
BONJOUR TOUT LE MONDE
```

Maintenant, vous pouvez choisir la méthode de capitalisation qui convient le mieux à vos besoins en fonction de la situation.

## Voir aussi
Pour en savoir plus sur les fonctions de manipulation de chaînes de caractères en Go, vous pouvez consulter la documentation officielle :  
- https://golang.org/pkg/strings/  
- https://tour.golang.org/basics/15  

Merci d'avoir lu cet article et à bientôt pour de nouvelles astuces sur le langage Go !