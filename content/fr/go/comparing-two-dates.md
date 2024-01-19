---
title:                "Comparer deux dates"
html_title:           "Clojure: Comparer deux dates"
simple_title:         "Comparer deux dates"
programming_language: "Go"
category:             "Go"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/go/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Quoi et Pourquoi?

Comparer deux dates, c'est déterminer si une date est antérieure, postérieure ou égale à une autre. En tant que programmeurs, nous faisons cela pour contrôler le flux de notre logique, par exemple pour vérifier si une date d'expiration est passée.

## Comment faire:

Dans Go, la méthode `After`, `Before` et `Equal` de l'objet `Time` permettent de comparer les dates. Par exemple:

```Go
package main

import (
	"fmt"
	"time"
)

func main() {
	t := time.Now()
	t2 := t.Add(time.Minute * 2)

	fmt.Println(t2.After(t))   // true
	fmt.Println(t2.Equal(t))   // false
	fmt.Println(t2.Before(t))  // false
}
```
Dans cet exemple, on crée d'abord une date `t` qui est l'instant présent, puis une date `t2` qui est 2 minutes plus tard. Lorsqu'on compare `t2` à `t`, on voit bien que `t2` est après `t`, donc `t2.After(t)` retourne `true`. 

## Deep Dive:

Historiquement, la gestion des dates a toujours été complexe en programmation. Go 1.0 ne proposait pas d'opérations de comparaison de dates aussi simples. Ce n'est qu'avec les versions ultérieures que des méthodes comme `After`, `Before` et `Equal` ont été ajoutées.

Il existe des alternatives à ces méthodes, notamment tester manuellement la différence entre deux dates, mais cela est généralement plus compliqué et plus sujet aux erreurs.

Il est important de noter que `time.Equal` n'est pas toujours équivalent à `!time.After` et `!time.Before` car `time.Equal` retourne `true` si les deux temps représentent le même instant, tandis que `!time.After` et `!time.Before` peuvent retourner `true` si les deux temps sont dans le même intervalle de temps.

## Voir aussi:

Pour en savoir plus sur le package `time` dans Go, consultez la documentation officielle:
- [Go time package](https://golang.org/pkg/time/)
- [Go by Example: Time](https://gobyexample.com/time)