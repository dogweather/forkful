---
title:    "Go: Obtenir la date actuelle"
keywords: ["Go"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fr/go/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Pourquoi

Obtenir la date actuelle peut sembler une tâche simple et banale, mais cela peut être très utile dans de nombreuses applications. Par exemple, afficher la date sur un site web ou enregistrer la date de création d'un fichier peut donner des informations importantes aux utilisateurs.

## Comment faire

Pour obtenir la date actuelle en Go, nous pouvons utiliser la fonction `now` du package `time`. Voici un exemple de code qui obtient la date actuelle et l'affiche à l'écran :

```Go
package main

import (
    "fmt"
    "time"
)

func main() {
    current := time.Now()
    fmt.Println("La date actuelle est :", current)
}
```

Si vous exécutez ce code, vous devriez voir une sortie similaire à :

```bash
La date actuelle est : 2021-11-17 14:30:12.488627 +0100 CET m=+0.000218784
```

Nous pouvons également personnaliser le format de la date en utilisant la méthode `Format` sur notre objet `Time`. Par exemple, si nous voulons afficher la date au format "dd/mm/yyyy", nous pouvons utiliser la chaîne de format `02/01/2006`, car c'est la date de la publication initiale de Go. Voici un exemple de code pour cela :

```Go
package main

import (
    "fmt"
    "time"
)

func main() {
    current := time.Now()
    fmt.Println("La date actuelle est :", current.Format("02/01/2006"))
}
```

La sortie de ce code devrait être :

```bash
La date actuelle est : 17/11/2021
```

## Plongée en profondeur

Maintenant que nous savons comment obtenir la date actuelle en Go, il est important de comprendre que cette date sera différente selon le fuseau horaire de l'ordinateur sur lequel le code s'exécute. Cela peut parfois prêter à confusion, mais cela peut être résolu en utilisant la méthode `UTC` sur notre objet `Time`.

Nous pouvons également manipuler la date en ajoutant ou en soustrayant des durées en utilisant les méthodes `Add` et `Sub` sur notre objet `Time`. Par exemple, pour obtenir la date d'il y a 1 an à partir de la date actuelle, nous pouvons utiliser :

```Go
package main

import (
    "fmt"
    "time"
)

func main() {
    current := time.Now()
    oneYearAgo := current.AddDate(-1, 0, 0)
    fmt.Println("Il y a un an, la date était :", oneYearAgo)
}
```

La sortie de ce code devrait être :

```bash
Il y a un an, la date était : 2020-11-17 14:30:12.488627 +0100 CET
```

Il existe également d'autres fonctions utiles dans le package `time`, comme `Hour`, `Minute` et `Second`, qui permettent d'obtenir des informations plus précises sur la date actuelle.

## Voir aussi

Il existe de nombreuses autres fonctions et outils en Go pour travailler avec les dates et les heures. Voici quelques liens utiles pour en savoir plus :

- [Documentation officielle sur le package `time`](https://golang.org/pkg/time/)
- [Exemples pratiques d'utilisation de la date et de l'heure en Go](https://yourbasic.org/golang/time-date-timezone/)
- [Un package Go pour manipuler facilement les dates (github.com)](https://github.com/jinzhu/now)