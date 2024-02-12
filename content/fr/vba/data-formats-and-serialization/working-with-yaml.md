---
title:                "Travailler avec YAML"
aliases:
- /fr/vba/working-with-yaml.md
date:                  2024-02-01T22:07:13.329542-07:00
model:                 gpt-4-0125-preview
simple_title:         "Travailler avec YAML"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/vba/working-with-yaml.md"
changelog:
  - 2024-02-01, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Ce qu'est YAML et pourquoi l'utiliser ?

YAML, qui signifie "YAML Ain't Markup Language" (YAML n'est pas un langage de balisage), est un langage de sérialisation de données lisible par l'homme, couramment utilisé pour les fichiers de configuration. Les programmeurs l'utilisent souvent en raison de sa simplicité et de sa lisibilité dans une multitude d'environnements de programmation, y compris dans le domaine du scripting de Visual Basic for Applications (VBA) pour améliorer l'interopérabilité, ainsi que le stockage et l'échange de données.

## Comment faire :

Travailler avec YAML dans VBA nécessite de comprendre comment analyser et convertir YAML dans un format que VBA peut facilement manipuler, habituellement des dictionnaires ou collections. Malheureusement, VBA ne prend pas nativement en charge l'analyse ou la sérialisation de YAML. Cependant, vous pouvez utiliser une combinaison d'outils de conversion JSON et d'objets de dictionnaire pour travailler avec des données YAML, compte tenu de la proche relation entre YAML et JSON.

Premièrement, convertissez vos données YAML en JSON en utilisant un convertisseur en ligne ou un outil de conversion YAML vers JSON dans votre environnement de développement. Une fois converti, vous pouvez utiliser l'exemple suivant pour analyser JSON en VBA, notant que cette approche vous permet indirectement de travailler avec YAML :

```vb
' Ajouter une référence à Microsoft Scripting Runtime pour Dictionary
' Ajouter une référence à Microsoft XML, v6.0 pour l'analyse JSON

Sub ParseYAMLAsJSON()
    Dim jsonText As String
    jsonText = "{""name"": ""John Doe"", ""age"": 30}" ' Ceci est du JSON converti à partir de YAML
    
    ' En supposant que vous ayez une fonction d'analyse JSON
    Dim parsedData As Dictionary
    Set parsedData = JsonParser(jsonText)
    
    Debug.Print "Nom : " & parsedData("name")
    Debug.Print "Âge : " & parsedData("age")
End Sub

Function JsonParser(ByVal jsonText As String) As Dictionary
    ' Placeholder pour la logique d'analyse JSON - vous pourriez utiliser une bibliothèque externe ici
    Set JsonParser = New Dictionary
    JsonParser.Add "name", "John Doe"
    JsonParser.Add "age", 30
End Function
```
Dans cet exemple, la fonction `JsonParser` est un substitut pour l'endroit où vous analyseriez le JSON. Diverses bibliothèques sont disponibles pour aider à l'analyse JSON, puisque les bibliothèques d'analyse directe de YAML pour VBA sont rares.

## Plongée profonde

L'absence de gestion directe YAML dans VBA peut être attribuée à son âge et à l'environnement pour lequel il a été conçu, qui n'était initialement pas conçu avec les formats modernes de sérialisation de données à l'esprit. YAML lui-même est apparu comme un format de configuration et de sérialisation populaire au début des années 2000, coïncidant avec l'avènement d'applications nécessitant des fichiers de configuration plus conviviaux pour les humains.

Les programmeurs utilisent généralement des outils ou bibliothèques externes pour combler le fossé entre VBA et YAML. Cela implique souvent de convertir YAML en JSON, comme montré, en raison du support JSON disponible via diverses bibliothèques et de la similarité entre JSON et YAML en termes de structure et de finalité.

Bien que travailler directement avec YAML dans VBA montre la flexibilité du langage, il est intéressant de noter que d'autres environnements de programmation (par exemple, Python ou JavaScript) fournissent un support plus natif et transparent pour YAML. Ces alternatives pourraient être mieux adaptées pour des projets fortement dépendants de YAML pour la configuration ou la sérialisation des données. Néanmoins, pour ceux qui sont engagés dans ou nécessitant VBA, la méthode indirecte via la conversion JSON reste une approche viable et utile pour gérer et manipuler des données YAML.
