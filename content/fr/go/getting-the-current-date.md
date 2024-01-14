---
title:                "Go: Obtenir la date actuelle"
programming_language: "Go"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/go/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Pourquoi
L'intérêt de comprendre comment obtenir la date actuelle en programmation en Go peut sembler évident, mais il y a de nombreuses nuances à prendre en compte. Que vous souhaitiez enregistrer la date dans une base de données, créer un calendrier dynamique ou simplement afficher la date pour les utilisateurs, savoir comment obtenir la date actuelle en Go est une compétence précieuse pour tout programmeur.

## Comment Faire
Il existe plusieurs façons d'obtenir la date actuelle en Go, mais voici l'une des méthodes les plus simples :

```
package main

import (
	"fmt"
	"time"
)

func main() {
	currentTime := time.Now()
	fmt.Println("La date actuelle est :", currentTime)
}
```

La sortie de ce code sera quelque chose comme ceci :

```
La date actulle est : 2021-07-09 16:30:27.8408544 +0000 UTC m=+0.049004501
```

Cette méthode utilise la fonction `Now()` de la bibliothèque `time` pour obtenir l'heure actuelle, puis l'affiche à l'aide de la fonction `Println()` de la bibliothèque `fmt`.

Il est également possible de formater la date pour l'afficher dans un format spécifique, par exemple en utilisant la méthode `Format()` :

```
layout := "2006-01-02"
currentTime := time.Now().Format(layout)
fmt.Println("La date actuelle est :", currentTime)
```

Cela produira une sortie comme ceci :

```
La date actuelle est : 2021-07-09
```

Il existe de nombreuses autres fonctions utiles dans la bibliothèque `time` pour travailler avec les dates et les heures en Go. Consultez la documentation officielle pour en savoir plus sur ces fonctions.

## Plongée Profonde
Il est important de noter que la fonction `Now()` utilise le fuseau horaire UTC par défaut. Si vous souhaitez utiliser un fuseau horaire différent, vous pouvez le spécifier en utilisant la méthode `Location()` et en la passant à la fonction `Now()` :

```
location, err := time.LoadLocation("Europe/Paris")
if err != nil {
	fmt.Println(err)
}
currentTime := time.Now().In(location)
fmt.Println("La date actuelle en France est :", currentTime)
```

Cette méthode utilise la bibliothèque `time/zoneinfo` pour charger le fuseau horaire souhaité et l'applique à la date actuelle.

## Voir Aussi
- [Documentation officielle de la bibliothèque `time` en Go](https://pkg.go.dev/time)
- [Guide pour formater les dates en Go](https://www.alexedwards.net/blog/formatting-time-and-dates-in-go)
- [Guide pour manipuler les fuseaux horaires en Go](https://www.sohamkamani.com/golang/date-parsing/)