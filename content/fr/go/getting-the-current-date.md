---
title:                "Obtenir la date actuelle"
html_title:           "Bash: Obtenir la date actuelle"
simple_title:         "Obtenir la date actuelle"
programming_language: "Go"
category:             "Go"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/go/getting-the-current-date.md"
---

{{< edit_this_page >}}

# Allons directement au but: Manipuler les dates courantes en Go

## Quoi & Pourquoi?
Obtenir la date courante est un processus qui vous donne l'heure exacte à chaque instant. Les programmeurs en ont besoin pour des tâches comme le marquage des logs, la génération des timestamps, etc.

## Comment faire:
Obtenir la date courante en Go est assez simple. Jetez un coup d’œil au bout de code ci-dessous:

```Go
package main

import (
	"fmt"
	"time"
)

func main() {
	fmt.Println("La date et l'heure courante est:", time.Now())
}
```
Quand vous exécutez ce code, vous obtiendrez une sortie ressemblant à ceci:

```Go
La date et l'heure courante est: 2022-01-01 00:00:00.000000 +0000 UTC
```
## Plongée en profondeur
En abordant l'histoire, les dates et les heures ont toujours été cruciales en programmation dès les premières machines jusqu'aux systèmes actuels. 

Concernant les alternatives, Go dispose d'autres bibliothèques comme "Joda-Time" qui peut être utilisée pour un besoin spécifique, mais "time" reste la bibliothèque standard.

En termes d'implémentation, "time.Now()" interroge le système d'exploitation pour obtenir l'heure courante et la convertit au format approprié.

## Voir aussi 
Pour plus d'informations:
 
1. Documentation Go sur le package 'time': https://golang.org/pkg/time/
2. Tutoriel sur la gestion du temps en Go: https://www.golangprograms.com/golang-program-for-working-with-dates.html
3. Plus sur le temps et les dates en Go: https://yourbasic.org/golang/time-date-format/