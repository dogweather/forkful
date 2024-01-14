---
title:    "Go: Obtenir la date actuelle"
keywords: ["Go"]
---

{{< edit_this_page >}}

## Pourquoi

Il est important de pouvoir obtenir la date actuelle dans nos programmes Go, car cela nous permet de suivre le temps et d'effectuer des actions en fonction de la date. Que ce soit pour afficher la date dans une application, ou pour automatiser certaines tâches en fonction du jour, avoir accès à la date actuelle est une fonctionnalité essentielle dans la programmation.

## Comment faire

Pour obtenir la date actuelle en Go, nous utilisons la fonction `Now()` de la bibliothèque `time`. Voici un exemple de code pour imprimer la date actuelle dans la console :

```Go
package main

import (
    "fmt"
    "time"
)

func main() {
    now := time.Now()
    fmt.Println(now)
}
```

Le résultat de ce code sera quelque chose comme : `2021-01-01 12:00:00 +0000 UTC`. Il est important de noter que la date et l'heure seront différentes selon votre fuseau horaire.

Nous pouvons également utiliser la fonction `Format()` pour personnaliser le format de la date. Voici un exemple de code pour imprimer la date au format jour/mois/année :

```Go
package main

import (
    "fmt"
    "time"
)

func main() {
    now := time.Now().Format("02/01/2006")
    fmt.Println(now)
}
```

Le résultat sera alors `01/01/2021` en fonction de la date actuelle à laquelle vous exécutez le code.

## Plongée profonde

En plongeant davantage dans la bibliothèque `time`, nous pouvons également obtenir des informations plus précises sur la date actuelle. Par exemple, la méthode `Date()` nous permet d'obtenir l'année, le mois et le jour séparément. Voici un exemple de code pour imprimer l'année actuelle :

```Go
package main

import (
    "fmt"
    "time"
)

func main() {
    year := time.Now().Date()
    fmt.Println(year)
}
```

Le résultat sera une année de type `int` comme `2021`.

## Voir aussi

- [Documentation de la bibliothèque "time" en Go](https://golang.org/pkg/time/)
- [Autres façons d'utiliser la fonction `Now()` en Go](https://stackoverflow.com/questions/588004/is-there-a-way-to-get-the-current-time-in-nanoseconds-using-go)