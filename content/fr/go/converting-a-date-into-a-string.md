---
title:    "Go: Transformer une date en une chaîne de caractères"
keywords: ["Go"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fr/go/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Pourquoi

Lorsque vous travaillez avec des dates en programmation, il est très utile de pouvoir les convertir en chaînes de caractères pour les afficher ou les utiliser dans des opérations. Dans cet article, nous allons voir comment convertir une date en une chaîne de caractères en utilisant le langage de programmation Go.

## Comment faire

```Go
package main

import (
    "fmt"
    "time"
)

func main() {
    // Création d'une date au format time.Time
    date := time.Date(2021, time.April, 19, 0, 0, 0, 0, time.UTC)
    
    // Conversion en chaîne de caractères
    dateString := date.Format("02/01/2006")
    
    // Affichage du résultat
    fmt.Println(dateString)
}
```

Le code ci-dessus va créer une date correspondant au 19 avril 2021 et la convertir en une chaîne de caractères au format jour/mois/année. La méthode `Format` de l'objet `time.Time` permet de spécifier le format de la chaîne de sortie en utilisant des codes spécifiques pour chaque élément de la date (jour, mois, année, etc.).

Vous pouvez également utiliser d'autres codes, tels que `02/01/2006 15:04:05` pour afficher la date avec l'heure et les minutes, ou encore `01-02-06` pour un format différent.

## Plongée en profondeur

En plus des codes de formatage, il existe une autre méthode très utile pour la conversion de dates en chaînes de caractères : `Parse`. Cette méthode permet de convertir une chaîne de caractères en une date, en utilisant un format de référence.

Par exemple, si nous avons une chaîne de caractères `"2021-04-19"` et que nous voulons la convertir en date, nous pouvons utiliser la méthode `Parse` en spécifiant le format de référence correspondant : `2006-01-02`. Cette méthode va alors analyser la chaîne et renvoyer un objet `time.Time` correspondant.

Cette méthode peut s'avérer très utile lorsqu'on manipule des données provenant de différentes sources, avec des formats de dates différents.

## Voir aussi

- La documentation officielle de Go sur la gestion des dates : https://golang.org/pkg/time/
- Un tutoriel complet sur la conversion de dates en chaînes de caractères en Go : https://www.calhoun.io/using-parsedatetime-in-go/
- La documentation de la méthode `Parse` en Go : https://golang.org/pkg/time/#Parse