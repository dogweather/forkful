---
title:                "Calculer une date dans le futur ou le passé"
html_title:           "Go: Calculer une date dans le futur ou le passé"
simple_title:         "Calculer une date dans le futur ou le passé"
programming_language: "Go"
category:             "Go"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/go/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Quoi & Pourquoi ?
Calculer une date future ou passée est l'action de déterminer une date relative à une autre date donnée. Les programmeurs font cela pour gérer les événements programmés, les logs, les enregistrements de données et pour résoudre divers problèmes de temps dans l'application.

## Comment faire:
Voici un exemple simple de calcul d'une date future en utilisant le package `time` de Go :

```Go
package main

import (
    "fmt"
    "time"
)

func main() {
    maintenant := time.Now()
    fmt.Println("Maintenant:", maintenant)

    dateFuture := maintenant.AddDate(0, 0, 5)
    fmt.Println("Dans 5 jours:", dateFuture)
}
```

Et voici la sortie attendue :

```Go
Maintenant: 2021-12-14 12:30:00 +0000 UTC m=+0.000000001
Dans 5 jours: 2021-12-19 12:30:00 +0000 UTC m=+0.000000001
```

## Plongeon en profondeur
Historiquement, les langages de programmation ne fournissent pas toujours des outils intégrés pour gérer facilement le temps et les dates. Cependant, Go offre des fonctionnalités avancées pour manipuler les dates, ce qui facilite grandement la tâche aux programmeurs.

Il existe d'autres méthodes pour calculer une date future ou passée, par exemple en utilisant la fonction `Add` qui permet d'ajouter des durées spécifiées en heures, minutes, secondes, etc.

Le package `time` de Go fournit un ensemble de méthodes pour manipuler les dates et les temps. Il utilise le calendrier grégorien pour les calculs, sauf pour les calculs impliquant l'ajout ou la soustraction d'heures, minutes et secondes.

## Voir aussi
- Documentation Go sur le package `time` : https://golang.org/pkg/time/
- Article StackOverflow sur l'ajout de jours à la date actuelle en Go : https://stackoverflow.com/questions/37696740/add-days-to-current-date-in-golang
- Tutorial Go sur le formatage et le parsing de la date et du temps : https://www.golangprograms.com/go-language/date-and-time.html