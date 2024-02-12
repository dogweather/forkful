---
title:                "Refonte"
aliases:
- /fr/vba/refactoring.md
date:                  2024-02-01T21:59:45.959980-07:00
model:                 gpt-4-0125-preview
simple_title:         "Refonte"
tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/vba/refactoring.md"
changelog:
  - 2024-02-01, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Quoi et Pourquoi ?

Le refactoring en programmation implique de modifier la structure du code sans en changer le comportement, afin d'améliorer des aspects comme la lisibilité, la maintenabilité ou la performance. Les programmeurs refactorisent pour rendre le code plus efficace, plus facile à comprendre, plus facile à modifier à l'avenir, et pour réduire la probabilité de bugs.

## Comment faire :

Prenons un exemple basique en Visual Basic pour Applications (VBA) où nous avons une subroutine qui imprime les détails d'un employé. Initialement, le code est encombré, difficile à maintenir ou à étendre.

```vb
Sub PrintEmployeeDetails()
    Dim name As String
    Dim age As Integer
    Dim department As String
    name = "John Doe"
    age = 30
    department = "IT"
    
    MsgBox "Nom : " & name & vbCrLf & "Âge : " & age & vbCrLf & "Département : " & department
End Sub
```

Étape de refactoring 1 : Extraire la méthode. Une des techniques de refactoring les plus courantes consiste à prendre un morceau de code spécifique et à le déplacer dans sa propre méthode. Cela rend le code plus modulaire et plus facile à comprendre.

```vb
Sub PrintEmployeeDetails()
    Dim name As String
    Dim age As Integer
    Dim department As String
    name = "John Doe"
    age = 30
    department = "IT"
    
    DisplayMessage name, age, department
End Sub

Private Sub DisplayMessage(name As String, age As Integer, department As String)
    MsgBox "Nom : " & name & vbCrLf & "Âge : " & age & vbCrLf & "Département : " & department
End Sub
```

Étape de refactoring 2 : Utiliser une structure. Cette étape implique d'utiliser une structure de données pour contenir des données liées, améliorant la clarté du code et facilitant le passage de données groupées.

```vb
Type Employe
    name As String
    age As Integer
    department As String
End Type

Sub PrintEmployeeDetails()
    Dim emp As Employe
    emp.name = "John Doe"
    emp.age = 30
    emp.department = "IT"
    
    DisplayMessage emp
End Sub

Private Sub DisplayMessage(emp As Employe)
    MsgBox "Nom : " & emp.name & vbCrLf & "Âge : " & emp.age & vbCrLf & "Département : " & emp.department
End Sub
```

Ces étapes transforment un code encombré en un code modulaire et structuré, améliorant significativement la lisibilité et la maintenabilité.

## Approfondissement

Le concept de refactoring est aussi vieux que la programmation elle-même, mais c'est le livre de Martin Fowler "Refactoring : Améliorer la conception du code existant" qui l'a popularisé, en soulignant son importance dans le processus de développement logiciel. En Visual Basic pour Applications, le refactoring peut être quelque peu plus difficile en raison de l'absence d'outils intégrés trouvés dans les environnements de développement intégrés (IDE) plus modernes qui prennent en charge le refactoring automatisé.

Cependant, cela ne diminue pas son importance. Même en VBA, l'application manuelle des techniques de refactoring de base peut grandement améliorer la base de code, la rendant plus propre et plus efficace. Bien que VBA n'ait pas les mêmes commodités modernes, les principes d'une bonne conception de code restent universels. Les développeurs venant d'autres langages peuvent trouver le processus manuel fastidieux, mais apprécieront sans aucun doute les avantages d'investir du temps dans l'amélioration de la qualité du code dès le début.

Pour des environnements de développement plus robustes ou lors de travaux sur des projets particulièrement sophistiqués, il pourrait être judicieux d'explorer des alternatives offrant des outils de refactoring plus puissants ou de convertir des projets VBA en un langage .NET où Visual Studio fournit un support de refactoring étendu. Néanmoins, comprendre et appliquer les principes de refactoring en VBA est une compétence précieuse qui souligne l'importance d'écrire un code propre et maintenable, quel que soit l'environnement.
