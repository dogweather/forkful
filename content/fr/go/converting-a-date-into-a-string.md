---
title:                "Convertir une date en chaîne de caractères"
html_title:           "Go: Convertir une date en chaîne de caractères"
simple_title:         "Convertir une date en chaîne de caractères"
programming_language: "Go"
category:             "Go"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/go/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Pourquoi

Si vous êtes un développeur utilisant Go pour créer une application, il est très probable que vous ayez besoin de convertir des dates en chaînes de caractères pour afficher à vos utilisateurs ou pour stocker dans une base de données. Cela peut sembler simple, mais cela peut être un peu délicat si vous ne savez pas comment le faire correctement. Heureusement, Go a des fonctions intégrées pour faciliter la conversion de dates en chaînes de caractères, ce qui vous permet de gagner du temps et de réduire les erreurs potentielles.

## Comment faire

Pour convertir une date en chaîne de caractères, vous pouvez utiliser la méthode `Format` du package `time`. Voici un exemple de code:

```Go 
package main

import (
	"fmt"
	"time"
)

func main() {
	date := time.Date(2021, time.April, 15, 0, 0, 0, 0, time.UTC)
	str := date.Format("02 Jan 2006") // la date sera au format "15 Apr 2021"
	fmt.Println(str)
}
```

Le code ci-dessus utilise la méthode `Date` pour créer une date avec l'année, le mois et le jour spécifiés. Ensuite, la méthode `Format` est utilisée pour convertir la date en une chaîne de caractères avec le format "jour mois année". Il existe de nombreuses options de formatage disponibles, telles que "02/01/2006" pour une date au format numérique et "Monday, January 2, 2006" pour une date plus lisible.

## Plongée en profondeur

En examinant de plus près le code précédent, vous remarquerez que les années, mois et jours doivent être écrits en lettres majuscules dans la méthode `Format`. Cela est dû au fait que Go utilise un système de formats basé sur la date de référence du 2 janvier 2006. Vous pouvez également utiliser des formats personnalisés avec des lettres minuscules, mais cela nécessite un peu plus d'efforts de votre part.

De plus, le package `time` propose également des fonctions telles que `Parse` pour convertir une chaîne de caractères en date, et `Unix` pour convertir une date en timestamp unix. Vous pouvez utiliser ces fonctions pour effectuer des opérations plus avancées sur les dates.

## Voir aussi

Pour en savoir plus sur la conversion des dates en chaînes de caractères en Go, vous pouvez consulter les ressources suivantes:

- Documentation officielle sur le package `time`: https://golang.org/pkg/time/
- Tutoriel sur les formats de dates en Go: https://zetcode.com/golang/date-time/
- Exemples de code pour la conversion des dates en chaînes de caractères: https://gobyexample.com/time-formatting-parsing