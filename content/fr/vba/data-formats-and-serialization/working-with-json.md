---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 22:05:54.464233-07:00
description: "JSON (JavaScript Object Notation) est un format l\xE9ger d'\xE9change\
  \ de donn\xE9es facile \xE0 lire et \xE0 \xE9crire pour les humains, ainsi qu'\xE0\
  \ parser et \xE0 g\xE9n\xE9rer pour\u2026"
lastmod: '2024-03-13T22:44:57.606635-06:00'
model: gpt-4-0125-preview
summary: "JSON (JavaScript Object Notation) est un format l\xE9ger d'\xE9change de\
  \ donn\xE9es facile \xE0 lire et \xE0 \xE9crire pour les humains, ainsi qu'\xE0\
  \ parser et \xE0 g\xE9n\xE9rer pour\u2026"
title: Travailler avec JSON
---

{{< edit_this_page >}}

## Quoi et Pourquoi ?

JSON (JavaScript Object Notation) est un format léger d'échange de données facile à lire et à écrire pour les humains, ainsi qu'à parser et à générer pour les machines. Les programmeurs utilisent JSON pour transmettre des données entre un serveur et une application web ou pour stocker des informations de manière structurée et accessible dans divers environnements de programmation, y compris Visual Basic pour Applications (VBA).

## Comment faire :

VBA ne prend pas nativement en charge l'analyse ou la génération de JSON, donc nous utiliserons un langage de script comme JScript (via l'objet ScriptControl) pour parser les chaînes JSON et construire des objets JSON. Voici comment vous pouvez parser une chaîne JSON en VBA :

```basic
Function ParseJSON(ByVal jsonString As String) As Object
    Dim scriptControl As Object
    Set scriptControl = CreateObject("MSScriptControl.ScriptControl")
    scriptControl.Language = "JScript"
    
    scriptControl.Eval "var obj = (" & jsonString & ")"
    Set ParseJSON = scriptControl.CodeObject.obj
End Function

Sub DemoParseJSON()
    Dim jsonString As String
    jsonString = "{""name"":""John"", ""age"":30, ""city"":""New York""}"
    
    Dim parsed As Object
    Set parsed = ParseJSON(jsonString)
    
    MsgBox "Nom : " & parsed.name & ", Âge : " & parsed.age & ", Ville : " & parsed.city
End Sub
```

Pour générer du JSON, vous pourriez utiliser une approche similaire, en construisant la chaîne JSON par concaténation :

```basic
Function GenerateJSON(name As String, age As Integer, city As String) As String
    GenerateJSON = "{""name"":""" & name & """, ""age"":" & age & ", ""city"":""" & city & """}"
End Function

Sub DemoGenerateJSON()
    Dim jsonString As String
    jsonString = GenerateJSON("Jane", 28, "Los Angeles")
    
    MsgBox jsonString
End Sub
```

## Plongée Profonde

Les approches présentées exploitent le ScriptControl pour gérer JSON, sous-traitant essentiellement le travail à un moteur JavaScript. C'est une solution de contournement créative mais pas nécessairement la façon la plus efficace ou moderne de travailler avec JSON dans un contexte VBA. Dans des applications plus complexes, cette méthode pourrait devenir encombrante et introduire un surcoût de performance ou des préoccupations de sécurité, puisque le ScriptControl s'exécute dans un environnement qui a un accès complet à l'ordinateur hôte.

D'autres environnements de programmation, tels que Python ou JavaScript, offrent un support intégré pour JSON, les rendant plus adaptés pour des applications nécessitant une manipulation extensive de JSON. Ces langages fournissent des bibliothèques complètes qui facilitent non seulement le parsing et la génération mais aussi l'interrogation et la mise en forme des données JSON.

Malgré ces limitations dans VBA, comprendre comment travailler avec JSON est vital dans un monde où l'échange de données basé sur le web et les fichiers de configuration sont principalement formatés en JSON. Pour les programmeurs VBA, maîtriser ces techniques ouvre des opportunités pour l'intégration avec des API web, l'interprétation de fichiers de configuration, ou même la création d'applications web simples. Cependant, lorsque les projets augmentent en complexité ou exigent des performances élevées, les développeurs pourraient envisager d'utiliser des environnements de programmation plus adaptés à JSON.
