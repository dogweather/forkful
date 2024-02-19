---
aliases:
- /fr/vba/working-with-xml/
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 22:06:59.197227-07:00
description: "Travailler avec XML dans Visual Basic pour Applications (VBA) implique\
  \ l'analyse, la cr\xE9ation et la modification de documents XML dans le contexte\
  \ des\u2026"
lastmod: 2024-02-18 23:09:08.608530
model: gpt-4-0125-preview
summary: "Travailler avec XML dans Visual Basic pour Applications (VBA) implique l'analyse,\
  \ la cr\xE9ation et la modification de documents XML dans le contexte des\u2026"
title: Travailler avec XML
---

{{< edit_this_page >}}

## Quoi & Pourquoi ?

Travailler avec XML dans Visual Basic pour Applications (VBA) implique l'analyse, la création et la modification de documents XML dans le contexte des applications Microsoft Office. Les programmeurs se tournent vers cette capacité pour intégrer les applications Office avec des services web ou d'autres sources de données qui produisent du XML, facilitant ainsi l'échange de données et les fonctions de rapport.

## Comment faire :

Pour commencer à interagir avec XML, on utilise généralement l'objet `MSXML2.DOMDocument`. Cette interface vous permet de charger, d'analyser et de naviguer dans les documents XML. Voici un exemple simple montrant comment charger un fichier XML, naviguer dans sa structure et lire les attributs et le contenu textuel.

```basic
' Tout d'abord, assurez-vous d'avoir ajouté la référence à "Microsoft XML, v6.0" via Outils -> Références
Dim xmlDoc As MSXML2.DOMDocument60
Set xmlDoc = New MSXML2.DOMDocument60
xmlDoc.async = False
xmlDoc.Load("C:\Chemin\Vers\Votre\Fichier.xml") ' Chargez votre fichier XML

' Vérifiez si le XML a été chargé avec succès
If xmlDoc.parseError.ErrorCode <> 0 Then
    MsgBox "Erreur lors du chargement du XML :" & xmlDoc.parseError.reason
Else
    ' Naviguez et lisez les éléments
    Dim book As IXMLDOMNode
    Set book = xmlDoc.SelectSingleNode("//book/title") ' XPath pour trouver le premier <title> dans <book>
    MsgBox book.Text ' Affiche le texte du titre
End If
```

Dans le code d'exemple ci-dessus, nous créons une instance de `MSXML2.DOMDocument60`, chargeons un fichier XML, puis vérifions les erreurs. Si aucune erreur n'est trouvée, nous naviguons vers un nœud spécifique en utilisant XPath et affichons son contenu textuel.

## Plongée profonde :

L'intégration des capacités XML dans VBA remonte au début des années 2000, lorsque le besoin pour les applications Office d'interagir avec les données et services web a commencé à croître. La bibliothèque `MSXML`, ou Microsoft XML Core Services, a évolué au fil des ans, `MSXML2.DOMDocument60` étant l'une des dernières versions recommandées pour son utilisation en raison de ses performances et fonctionnalités de sécurité améliorées.

Bien que puissantes, les capacités de manipulation XML de VBA sont considérées comme moins efficaces et plus encombrantes par rapport aux environnements de programmation modernes tels que XML.etree de Python ou LINQ to XML de C#. La verbosité inhérente à VBA et la nécessité d'ajouter et de gérer manuellement les références peuvent décourager le développement rapide. En outre, avec l'avènement de JSON comme format d'échange de données plus léger, de nombreux programmeurs et applications se détournent du XML à moins que la nécessité d'interopérabilité avec des systèmes hérités ou des services d'entreprise spécifiques ne nécessite son utilisation.

Cependant, pour les tâches nécessitant l'analyse ou la génération de documents XML dans le contexte de l'automatisation des applications Microsoft Office, tirer parti des fonctionnalités de manipulation XML de VBA reste une approche viable et parfois nécessaire. Cela établit un équilibre entre l'accès à l'ensemble de fonctionnalités riches des applications Office et les capacités de manipulation de données structurées fournies par XML.
