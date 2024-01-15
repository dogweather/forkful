---
title:                "Majuscule d'une chaîne de caractères"
html_title:           "Go: Majuscule d'une chaîne de caractères"
simple_title:         "Majuscule d'une chaîne de caractères"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/go/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Pourquoi

Si vous travaillez avec des chaînes de caractères en Go, vous pourriez être confronté à un problème fréquent : comment rendre la première lettre de chaque mot en majuscule ? Dans cet article, je vais vous montrer comment capitaliser une chaîne de caractères en utilisant la puissance de Go.

## Comment faire


Le moyen le plus simple de capitaliser une chaîne de caractères en Go est d'utiliser la fonction `strings.Title()`. Elle prend une chaîne de caractères en entrée et renvoie une nouvelle chaîne de caractères avec la première lettre de chaque mot en majuscule. Voici un exemple de code : 

```Go
package main

import (
	"fmt"
	"strings"
)

func main() {
	str := "hello world"
	capitalizedStr := strings.Title(str)
	fmt.Println(capitalizedStr)
}
```
La sortie de ce code sera "Hello World". Comme vous pouvez le voir, la fonction `strings.Title()` a automatiquement converti la première lettre de chaque mot en majuscule.

## Plongée profonde

Maintenant que vous avez vu comment capitaliser une chaîne de caractères en utilisant `strings.Title()`, il peut être intéressant de comprendre comment cela fonctionne. En réalité, la fonction utilise plusieurs algorithmes pour capitaliser correctement une chaîne de caractères. Elle prend en compte des règles d'exceptions pour certains mots spéciaux comme "the", "and", "of" et bien d'autres. De plus, elle détecte également la casse déjà présente dans la chaîne de caractères et la conserve. Par exemple, si vous avez une chaîne de caractères comme "microsoft word", elle sera capitalisée en "Microsoft Word" et non en "Microsoft word". 

## Voir aussi

- [Documentation officielle de la fonction `strings.Title()`](https://golang.org/pkg/strings/#Title)
- [Tutoriel vidéo sur la gestion des chaînes de caractères en Go](https://www.youtube.com/watch?v=32TeZiFXjXk) 
- [Article sur les bonnes pratiques de programmation en Go](https://medium.com/@pcanino/bonnes-pratiques-de-programmation-en-go-d7357c5907a5)