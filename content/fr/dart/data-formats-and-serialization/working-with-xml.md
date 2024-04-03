---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:57:43.513828-07:00
description: "Comment faire : Dart n'inclut pas de support int\xE9gr\xE9 pour la gestion\
  \ XML dans sa biblioth\xE8que standard, n\xE9cessitant l'utilisation de paquets\
  \ tiers. Un\u2026"
lastmod: '2024-03-13T22:44:57.417683-06:00'
model: gpt-4-0125-preview
summary: "Dart n'inclut pas de support int\xE9gr\xE9 pour la gestion XML dans sa biblioth\xE8\
  que standard, n\xE9cessitant l'utilisation de paquets tiers."
title: Travailler avec XML
weight: 40
---

## Comment faire :
Dart n'inclut pas de support intégré pour la gestion XML dans sa bibliothèque standard, nécessitant l'utilisation de paquets tiers. Un paquet populaire est `xml`. Pour l'utiliser, vous devez d'abord l'ajouter à votre `pubspec.yaml` :

```yaml
dependencies:
  xml: ^5.0.0 // Utilisez la dernière version disponible
```

Ensuite, importez le paquet dans votre fichier Dart :

```dart
import 'package:xml/xml.dart' as xml;
```

**Analyse d'XML :**

Supposons que vous ayez une chaîne XML comme celle-ci :

```xml
<String name="greeting">Bonjour, monde !</String>
```

Vous pouvez analyser et lire l'XML comme suit :

```dart
void parseXml(String xmlString) {
    final document = xml.XmlDocument.parse(xmlString);
    final String content = document.findElements('String').single.getAttribute('name');
    print(content); // Affiche : greeting
}

void main() {
  final xmlString = '<String name="greeting">Bonjour, monde !</String>';
  parseXml(xmlString);
}
```

**Création de documents XML :**

La création d'un nouveau document XML est simple avec le paquet `xml` :

```dart
void createXml() {
  final builder = xml.XmlBuilder();
  builder.processing('xml', 'version="1.0"');
  builder.element('greeting', nest: () {
    builder.attribute('name', 'hello');
    builder.text('Bonjour, monde !');
  });
  final xmlDocument = builder.buildDocument();
  print(xmlDocument.toXmlString(pretty: true));
}

void main() {
  createXml();
}
```

**Sortie** :

```xml
<?xml version="1.0"?>
<greeting name="hello">Bonjour, monde !</greeting>
```

**Interrogation et modification d'XML :**

Pour trouver ou modifier des éléments, vous pouvez utiliser des méthodes semblables à XPath :

```dart
void modifyXml(String xmlString) {
    var document = xml.XmlDocument.parse(xmlString);
    var greeting = document.findAllElements('greeting').first;
    
    // Modification de l'attribut 'name'
    greeting.setAttribute('name', 'greeting_modified');
    
    // Ajout d'un nouvel élément enfant
    greeting.children.add(xml.XmlElement(xml.XmlName('message'), [], [xml.XmlText('Au revoir !')]));
    
    print(document.toXmlString(pretty: true));
}

void main() {
  final xmlString = '<greeting name="hello">Bonjour, monde !</greeting>';
  modifyXml(xmlString);
}
```

**Sortie** :

```xml
<greeting name="greeting_modified">
  Bonjour, monde !
  <message>Au revoir !</message>
</greeting>
```

Ces exemples démontrent les opérations de base pour travailler avec XML en Dart. Avec le paquet `xml`, vous pouvez analyser, créer et manipuler des documents XML pour répondre aux exigences de votre application.
