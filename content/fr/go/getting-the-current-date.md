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

# Qu'est-ce que c'est et pourquoi?
Obtenir la date actuelle est une tâche importante pour les programmeurs qui travaillent avec des données basées sur le temps. Cela leur permet de suivre les activités de l'utilisateur ou de planifier des tâches à exécuter à un moment précis.

# Comment faire:
Voici un exemple simple de code en Go pour obtenir la date actuelle:

```
import "fmt"
import "time"

func main() {
    fmt.Println("La date actuelle est:", time.Now())
}
```

Résultat:

```
La date actuelle est: 2020-09-04 10:08:15.880419 +0200 CEST m=+0.000114609
```

Vous pouvez également formater la date selon vos besoins en utilisant la méthode `Format` de la structure `time`:

```
date := time.Now()

fmt.Println(date.Format("02/01/2006"))
fmt.Println(date.Format("Jan 02, 2006"))
```

Résultat:

```
04/09/2020
Sep 04, 2020
```

# Plongée en profondeur:
Obtenir la date actuelle n'a pas toujours été aussi simple. Autrefois, les programmeurs devaient utiliser des fonctions complexes pour obtenir la date à partir du système d'exploitation. Avec l'utilisation répandue du langage Go, cette tâche est maintenant beaucoup plus facile.

Il existe également des alternatives pour obtenir la date actuelle en utilisant des packages tiers tels que le package `timeutil` ou `chrono`.

Dans le code, la date actuelle est représentée par une structure `time` qui contient des informations sur l'année, le mois, le jour, l'heure, les minutes et les secondes.

# Voir aussi:
- [Documentation officielle de la structure time en Go](https://golang.org/pkg/time/)
- [Package timeutil pour manipuler les dates en Go](https://github.com/leekchan/timeutil)
- [Package chrono pour gérer les temps en Go](https://github.com/xtuc/chrono)