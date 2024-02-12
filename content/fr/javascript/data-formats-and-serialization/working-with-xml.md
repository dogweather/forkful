---
title:                "Travailler avec XML"
aliases:
- /fr/javascript/working-with-xml.md
date:                  2024-01-26T04:32:32.326243-07:00
model:                 gpt-4-0125-preview
simple_title:         "Travailler avec XML"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/javascript/working-with-xml.md"
---

{{< edit_this_page >}}

## Quoi et Pourquoi ?
Travailler avec XML signifie analyser, manipuler et produire du contenu XML à l'aide de code. Les programmeurs le font parce que XML est largement utilisé pour les fichiers de configuration, l'échange de données et les services web en raison de sa nature lisible par l'homme et analysable par machine.

## Comment faire :

Voici comment analyser XML :

```javascript
let parser = new DOMParser();
let xmlString = `<note>
                    <to>Utilisateur</to>
                    <from>Auteur</from>
                    <heading>Rappel</heading>
                    <body>Ne m'oublie pas ce week-end !</body>
                 </note>`;

let xmlDoc = parser.parseFromString(xmlString, "application/xml");
console.log(xmlDoc.getElementsByTagName('to')[0].childNodes[0].nodeValue);
// Sortie : Utilisateur
```

Et pour produire du XML :

```javascript
let xmlDocument = document.implementation.createDocument('', '', null);
let noteElement = xmlDocument.createElement('note');
noteElement.appendChild(xmlDocument.createElement('to')).textContent = 'Utilisateur';
xmlDocument.appendChild(noteElement);
let serializer = new XMLSerializer();
let xmlString = serializer.serializeToString(xmlDocument);
console.log(xmlString);
// Sortie : <note><to>Utilisateur</to></note>
```

## Plongée en profondeur

XML est l'abréviation de eXtensible Markup Language, un format de données qui existe depuis la fin des années 90. Il définit un ensemble de règles pour l'encodage de documents lisibles à la fois par les humains et les machines. Historiquement, XML a gagné en popularité pour sa flexibilité et sa hiérarchie structurée, ce qui en faisait un choix pour les services web, comme SOAP, et de nombreux fichiers de configuration.

Les alternatives à XML incluent JSON (JavaScript Object Notation), qui est devenu populaire pour sa facilité d'utilisation avec JavaScript et sa légèreté. YAML est une autre alternative, appréciée pour être à la fois conviviale pour les humains et un choix courant pour la configuration.

XML est implémenté en JavaScript à l'aide des interfaces DOMParser et XMLSerializer. Le DOM XML (Document Object Model) permet de naviguer et d'éditer des documents XML tout comme vous le feriez avec HTML. Malgré l'ascension du JSON, comprendre XML est clé, car de nombreux systèmes hérités et industries spécifiques se reposent encore dessus pour l'échange de données.

## Voir également

- MDN Web Docs (Analyse XML) : https://developer.mozilla.org/en-US/docs/Web/API/DOMParser
- W3Schools (Tutoriel DOM XML) : https://www.w3schools.com/xml/dom_intro.asp
- "Qu'est-ce que XML ?" : https://www.w3.org/XML/
