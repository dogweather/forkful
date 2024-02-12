---
title:                "Analyser une date à partir d'une chaîne de caractères"
date:                  2024-02-01T21:57:36.661036-07:00
model:                 gpt-4-0125-preview
simple_title:         "Analyser une date à partir d'une chaîne de caractères"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/vba/parsing-a-date-from-a-string.md"
changelog:
  - 2024-02-01, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Quoi & Pourquoi ?

L'analyse d'une date à partir d'une chaîne en Visual Basic pour Applications (VBA) consiste à convertir un texte qui représente une date en un type de données date. Les programmeurs font cela pour manipuler plus efficacement les dates dans leurs applications, par exemple pour des comparaisons, des calculs ou des mises en forme.

## Comment faire :

VBA offre une méthode directe pour analyser une chaîne en une date à l'aide de la fonction `CDate` ou de la fonction `DateValue`. Cependant, il est crucial que la chaîne soit dans un format de date reconnaissable.

Voici un exemple simple utilisant `CDate` :

```basic
Sub ParseDateUsingCDate()
    Dim dateString As String
    Dim parsedDate As Date
    
    dateString = "2023-04-01"
    parsedDate = CDate(dateString)
    
    Debug.Print "Date analysée : "; parsedDate
End Sub
```

Si vous exécutez ce code, le résultat dans la fenêtre Immédiate (accessible via `Ctrl+G` dans l'éditeur VBA) serait :

```
Date analysée : 4/1/2023
```

Vous pouvez également utiliser la fonction `DateValue`, qui est plus spécifique aux dates (ignorant la partie heure) :

```basic
Sub ParseDateUsingDateValue()
    Dim dateString As String
    Dim parsedDate As Date
    
    dateString = "April 1, 2023"
    parsedDate = DateValue(dateString)
    
    Debug.Print "Date analysée avec DateValue : "; parsedDate
End Sub
```

L'exemple de sortie pour cela afficherait de manière similaire dans la fenêtre Immédiate :

```
Date analysée avec DateValue : 4/1/2023
```

Gardez à l'esprit que la réussite de l'analyse dépend de la conformité du format de la date de la chaîne avec les paramètres système ou de l'application.

## Exploration approfondie

À l'interne, lorsque VBA analyse une chaîne en une date, il utilise les paramètres régionaux du système d'exploitation Windows pour interpréter le format de la date. Cela est crucial à comprendre car une chaîne de date qui s'analyse parfaitement sur un système peut causer une erreur sur un autre s'ils utilisent des paramètres de date/heure différents.

Historiquement, la gestion des dates a été une source courante de bogues dans les applications, en particulier celles qui sont utilisées à l'international. Cette dépendance aux paramètres régionaux dans VBA est la raison pour laquelle certains pourraient envisager des alternatives comme le format ISO 8601 (par exemple, "AAAA-MM-JJ") pour une représentation et une analyse de date non ambiguës à travers différents systèmes. Malheureusement, VBA ne prend pas en charge nativement l'ISO 8601, et une analyse manuelle serait nécessaire pour une conformité stricte.

Pour une analyse de date complexe au-delà de ce que `CDate` ou `DateValue` peut gérer, ou pour garantir une analyse cohérente indépendamment des paramètres de localité du système, les programmeurs peuvent recourir à des fonctions d'analyse personnalisées. Celles-ci pourraient impliquer de diviser la chaîne de date en composants (année, mois, jour) et de construire une date en utilisant la fonction `DateSerial`. D'autres pourraient choisir des langages ou des bibliothèques plus puissants conçus avec l'internationalisation à l'esprit pour de telles tâches.
