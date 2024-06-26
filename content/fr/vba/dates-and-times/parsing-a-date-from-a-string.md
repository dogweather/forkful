---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 21:57:36.661036-07:00
description: "Comment faire : VBA offre une m\xE9thode directe pour analyser une cha\xEE\
  ne en une date \xE0 l'aide de la fonction `CDate` ou de la fonction `DateValue`.\u2026"
lastmod: '2024-03-13T22:44:57.591130-06:00'
model: gpt-4-0125-preview
summary: "VBA offre une m\xE9thode directe pour analyser une cha\xEEne en une date\
  \ \xE0 l'aide de la fonction `CDate` ou de la fonction `DateValue`."
title: "Analyser une date \xE0 partir d'une cha\xEEne de caract\xE8res"
weight: 30
---

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
