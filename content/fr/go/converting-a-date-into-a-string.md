---
title:                "Go: Conversion d'une date en chaîne de caractères"
simple_title:         "Conversion d'une date en chaîne de caractères"
programming_language: "Go"
category:             "Go"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/go/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Pourquoi

La conversion d'une date en chaîne de caractères est une fonctionnalité essentielle lorsque l'on travaille avec des dates en programmation. Cela permet d'afficher les dates de manière lisible pour les utilisateurs et de les manipuler plus facilement dans le code.

## Comment faire

Voici un exemple de code en Go pour convertir une date en chaîne de caractères :

```Go
package main

import (
    "fmt"
    "time"
)

func main() {
    // Définir une date
    date := time.Date(2021, time.March, 19, 19, 30, 0, 0, time.UTC)
    
    // Convertir en chaîne de caractères avec le format désiré
    dateString := date.Format("02-01-2006")
    
    // Afficher le résultat
    fmt.Println(dateString)
}
```

La sortie de ce code sera "19-03-2021", en utilisant la méthode "Format" avec le format "jour-mois-année". Il est possible de personnaliser le format en fonction des besoins, en utilisant différents éléments comme "02" pour le jour avec un zéro initial si nécessaire, "01" pour le mois avec un zéro initial si nécessaire et "2006" pour l'année.

## Profonde plongée

La méthode "Format" utilisée dans l'exemple précédent utilise des constantes de type "time" pour définir le format de la date en fonction de leur position. Ces constantes peuvent être utilisées pour créer des formats personnalisés en fonction de différents éléments tels que le jour, le mois, l'année, l'heure, etc.

Pour en savoir plus sur les différentes options disponibles pour le formatage de dates en Go, vous pouvez consulter la documentation officielle de la librairie "time" sur le site de Go.

## Voir aussi

- [Documentation officielle de la librairie "time" (en anglais)](https://pkg.go.dev/time)
- [Exemples de formats de dates en Go (en anglais)](https://yourbasic.org/golang/format-parse-string-time-date-example/#format-time-date-example)