---
aliases:
- /fr/google-apps-script/working-with-xml/
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 22:06:35.549546-07:00
description: "Travailler avec l'XML dans Google Apps Script permet aux programmeurs\
  \ de parser, de manipuler et de g\xE9n\xE9rer des donn\xE9es XML, essentielles pour\
  \ les\u2026"
lastmod: 2024-02-18 23:09:08.320003
model: gpt-4-0125-preview
summary: "Travailler avec l'XML dans Google Apps Script permet aux programmeurs de\
  \ parser, de manipuler et de g\xE9n\xE9rer des donn\xE9es XML, essentielles pour\
  \ les\u2026"
title: Travailler avec XML
---

{{< edit_this_page >}}

## Quoi & Pourquoi ?

Travailler avec l'XML dans Google Apps Script permet aux programmeurs de parser, de manipuler et de générer des données XML, essentielles pour les services web et les configurations. Les programmeurs adoptent cette approche pour s'intégrer aux systèmes hérités, effectuer du web scraping, ou communiquer avec de nombreuses API qui se fient encore à l'XML plutôt qu'au JSON pour l'échange de données.

## Comment :

Google Apps Script fournit le `XmlService` pour travailler avec des données XML. Ci-dessous, nous démontrons comment parser une chaîne XML, modifier son contenu et générer une nouvelle chaîne XML.

Parser une chaîne XML :

```javascript
function parseXML() {
  var xmlString = '<root><child name="first">Hello</child><child name="second">World</child></root>';
  var document = XmlService.parse(xmlString);
  var root = document.getRootElement();
  var children = root.getChildren('child');
  Logger.log(children[0].getText()); // Logs : Hello
}
```

Pour modifier l'XML, vous voudrez peut-être ajouter un nouvel élément enfant :

```javascript
function addNewChild() {
  var xmlString = '<root><child name="first">Hello</child></root>';
  var document = XmlService.parse(xmlString);
  var root = document.getRootElement();
  
  var newChild = XmlService.createElement('child').setText('World');
  root.addContent(newChild);
  
  var xml = XmlService.getPrettyFormat().format(document);
  Logger.log(xml);
  // Log la nouvelle chaîne XML avec l'élément enfant ajouté
}
```

Générer une chaîne XML à partir de zéro :

```javascript
function createXML() {
  var root = XmlService.createElement('root');
  var child = XmlService.createElement('child').setText('Hello World');
  root.addContent(child);
  
  var xml = XmlService.getPrettyFormat().format(XmlService.createDocument(root));
  Logger.log(xml);
  // Résultats : <root><child>Hello World</child></root>
}
```

## Approfondissement

Historiquement, l'XML (eXtensible Markup Language) était la norme de facto pour l'échange de données avant que le JSON n'émerge comme une alternative légère. La syntaxe verbeuse de l'XML et son modèle de parsing strict offraient un format de données robuste, bien que volumineux. Dans Google Apps Script, l'API `XmlService` encapsule la création, le parsing et la manipulation de données XML, reconnaissant son importance continue dans divers systèmes hérités et d'entreprise, les services web SOAP et les fichiers de configuration pour applications.

Malgré la prévalence du JSON dans le développement web moderne pour sa simplicité et sa facilité d'utilisation avec JavaScript, l'XML reste pertinent dans les domaines où la validation de documents et les hiérarchies structurées sont cruciales. Cependant, pour les nouveaux projets, surtout ceux orientés vers les API web, le JSON est souvent le choix le plus pratique en raison de sa nature légère et de son intégration transparente avec JavaScript.

Comprendre l'XML et son traitement dans Google Apps Script est primordial pour les développeurs travaillant dans des environnements où l'intégration avec des systèmes plus anciens ou des API d'entreprise spécifiques est nécessaire. Cependant, lors du démarrage de nouveaux projets ou lorsque la flexibilité est clé, évaluer la nécessité de l'XML par rapport à des alternatives comme le JSON est conseillé.
