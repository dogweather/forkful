---
title:                "Obtenir la date actuelle"
html_title:           "Go: Obtenir la date actuelle"
simple_title:         "Obtenir la date actuelle"
programming_language: "Go"
category:             "Go"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/go/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Pourquoi

Si vous êtes un programmeur Go, vous savez que les méthodes et fonctions offertes par ce langage sont conçues pour rendre le développement plus facile et plus efficace. Ainsi, obtenir la date actuelle est une tâche courante dans de nombreux programmes, et cela peut être réalisé en quelques lignes de code. Dans cet article, nous allons découvrir comment obtenir la date actuelle en Go et pourquoi cela peut être utile pour vos projets.

## Comment faire

Pour obtenir la date actuelle en Go, nous allons utiliser la fonction `Now()` de la bibliothèque `time`. Voici un exemple de code qui peut être utilisé pour obtenir la date actuelle et l'afficher dans la console:

```Go
package main

import (
    "fmt"
    "time"
)

func main() {
    date := time.Now()
    fmt.Println("La date actuelle est:", date.Format("02/01/2006"))
}
```

Le code ci-dessus utilise la méthode `Format` pour afficher la date dans un format spécifique. Le format "02/01/2006" est une convention dans Go pour représenter la date sous la forme JJ/MM/AAAA. Vous pouvez changer ce format selon vos préférences en utilisant d'autres symboles de formatage. La sortie de ce code sera la date actuelle au format "JJ/MM/AAAA":

```
La date actuelle est: 10/10/2021
```

Vous pouvez également obtenir uniquement le jour, le mois ou l'année en utilisant les méthodes `Day()`, `Month()` et `Year()` respectivement. Voici un exemple qui affiche uniquement le mois en cours:

```Go
package main

import (
    "fmt"
    "time"
)

func main() {
    date := time.Now()
    month := date.Month()
    fmt.Println("Nous sommes en:", month)
}
```

La sortie de ce code sera le mois actuel au format numérique:

```
Nous sommes en: 10
```

## Plongée en profondeur

Maintenant que vous savez comment obtenir la date actuelle en Go, il est important de comprendre comment cela fonctionne en interne. Lorsque vous utilisez la fonction `Now()`, Go utilise le fuseau horaire local de votre ordinateur pour déterminer l'heure et la date. Si vous souhaitez obtenir la date dans un fuseau horaire différent, vous pouvez utiliser la fonction `LoadLocation()` de la bibliothèque `time`.

Il est également intéressant de noter que la fonction `Now()` renvoie une valeur de type `time.Time`, qui contient les informations sur la date et l'heure. La méthode `Format()` permet ensuite de formater ces informations selon nos besoins.

## Voir aussi

Maintenant que vous savez comment obtenir la date actuelle en Go, vous pouvez aller plus loin en explorant les autres méthodes et fonctions offertes par la bibliothèque `time`. Voici quelques ressources utiles pour vous aider à approfondir vos connaissances:

- [Documentation officielle de la bibliothèque `time`](https://golang.org/pkg/time/)
- [Tutoriel vidéo sur la manipulation des dates en Go](https://www.youtube.com/watch?v=0Q4cPVG3fmw)
- [Article sur le formatage des dates en Go](https://www.digitalocean.com/community/tutorials/how-to-work-with-date-and-time-in-go-fr)

Maintenant que vous savez comment obtenir la date actuelle en Go, vous pouvez l'utiliser dans vos projets pour afficher des informations temporelles ou pour exécuter des tâches à des moments spécifiques. Gardez ces connaissances à l'esprit et continuez à explorer les différentes fonctionnalités offertes par Go pour rendre votre code encore plus efficace. Bon codage !