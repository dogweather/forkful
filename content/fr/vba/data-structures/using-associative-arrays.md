---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 22:04:23.500599-07:00
description: "Les tableaux associatifs, souvent connus sous le nom de dictionnaires\
  \ dans Visual Basic pour Applications (VBA), permettent aux programmeurs de cr\xE9\
  er des\u2026"
lastmod: '2024-03-13T22:44:57.548862-06:00'
model: gpt-4-0125-preview
summary: "Les tableaux associatifs, souvent connus sous le nom de dictionnaires dans\
  \ Visual Basic pour Applications (VBA), permettent aux programmeurs de cr\xE9er\
  \ des collections de paires cl\xE9-valeur."
title: Utilisation des tableaux associatifs
weight: 15
---

## Quoi et Pourquoi ?

Les tableaux associatifs, souvent connus sous le nom de dictionnaires dans Visual Basic pour Applications (VBA), permettent aux programmeurs de créer des collections de paires clé-valeur. Cette fonctionnalité est cruciale pour un stockage et une récupération de données efficaces, offrant une manière plus flexible et intuitive de gérer les données par rapport aux indices de tableaux traditionnels.

## Comment faire :

Dans VBA, l'objet `Dictionary` offre une fonctionnalité similaire aux tableaux associatifs. Vous devez d'abord ajouter une référence au Microsoft Scripting Runtime pour l'utiliser :

1. Dans l'éditeur VBA, allez dans Outils > Références...
2. Cochez "Microsoft Scripting Runtime" et cliquez sur OK.

Voici comment déclarer, remplir et accéder aux éléments dans un `Dictionary` :

```vb
Dim sampleDictionary As Dictionary
Set sampleDictionary = New Dictionary

' Ajout d'éléments
sampleDictionary.Add Key:="Name", Item:="John Doe"
sampleDictionary.Add Key:="Age", Item:=29
sampleDictionary.Add Key:="Occupation", Item:="Ingénieur"

' Accès aux éléments
Debug.Print sampleDictionary.Item("Name")  ' Résultat : John Doe
Debug.Print sampleDictionary.Item("Age")   ' Résultat : 29

' Vérification de l'existence d'une clé
If sampleDictionary.Exists("Occupation") Then
    Debug.Print "La clé Occupation existe"
End If

' Suppression d'éléments
sampleDictionary.Remove("Occupation")

' Parcours du dictionnaire
For Each Key In sampleDictionary.Keys
    Debug.Print Key & ": " & sampleDictionary.Item(Key)
Next Key
```

## Exploration approfondie

L'objet `Dictionary` interagit en coulisse avec des composants du Windows Scripting Host. Ainsi, c'est un objet COM à liaison tardive, qui était une manière courante d'étendre la fonctionnalité de VBA dans le passé. Son utilisation dans VBA peut considérablement améliorer la capacité du langage à manipuler des ensembles de données complexes sans imposer une structure rigide, comme c'est le cas avec les tableaux traditionnels ou les plages Excel.

Une limitation à garder à l'esprit est que l'accès au `Dictionary` nécessite de définir une référence au Microsoft Scripting Runtime, ce qui peut compliquer la distribution de vos projets VBA. Des alternatives comme les Collections existent dans VBA mais manquent de certaines des caractéristiques clés du `Dictionary`, comme la capacité de vérifier facilement l'existence d'une clé sans déclencher d'erreur.

Dans des contextes de programmation plus récents, des langages comme Python offrent un support intégré pour les tableaux associatifs (également connus sous le nom de dictionnaires en Python) sans nécessiter de références externes. Ce support intégré simplifie le processus et offre des fonctionnalités plus avancées dès le départ. Cependant, dans le cadre de VBA et pour des applications spécifiques destinées à automatiser des tâches dans la suite Microsoft Office, l'utilisation de l'objet `Dictionary` reste une méthode puissante et pertinente pour des structures de données de type tableau associatif.
