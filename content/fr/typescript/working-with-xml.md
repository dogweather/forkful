---
title:                "Travailler avec XML"
date:                  2024-01-26T04:36:01.398063-07:00
model:                 gpt-4-0125-preview
simple_title:         "Travailler avec XML"

category:             "TypeScript"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/typescript/working-with-xml.md"
---

{{< edit_this_page >}}

## Quoi et pourquoi ?
Travailler avec XML signifie analyser, manipuler et écrire des données XML en utilisant la programmation. Les programmeurs gèrent l'XML pour échanger des données entre différents systèmes, pour les fichiers de configuration, ou lorsqu'ils travaillent avec des normes comme SOAP qui reposent sur XML.

## Comment faire :
```TypeScript
import { parseString } from 'xml2js';

// Exemple d'XML
const xml = `<note>
                <to>Utilisateur</to>
                <from>Auteur</from>
                <heading>Rappel</heading>
                <body>N'oubliez pas la réunion !</body>
             </note>`;

// Analyser l'XML en JSON
parseString(xml, (err, result) => {
    if(err) throw err;
    console.log(result);
});

// En supposant que l'analyse ait été réussie, la sortie pourrait ressembler à :
// { note:
//    { to: ['Utilisateur'],
//      from: ['Auteur'],
//      heading: ['Rappel'],
//      body: ['N'oubliez pas la réunion !'] } 
}
```

## Plongée profonde
XML, ou Extensible Markup Language, existe depuis la fin des années 90. Sa nature auto-descriptive et son format lisible par l'homme en ont fait un succès dès le début pour diverses applications telles que les flux RSS, la gestion de configuration, et même les formats de documents de bureau comme Microsoft Office Open XML. Mais, il est verbeux comparé au JSON, et la tendance s'est inversée. JSON a obtenu les projecteurs pour les API basées sur le web en raison de sa légèreté et de sa compatibilité native avec JavaScript.

Néanmoins, XML n'est pas mort. Il est utilisé dans des systèmes d'entreprise à grande échelle et pour des normes de documents qui ne sont pas passées au JSON. Des outils comme `xml2js` pour TypeScript ou `lxml` en Python prouvent qu'il y a un besoin continu de manipulation d'XML en programmation.

TypeScript n'a pas de support intégré pour XML comme il le fait pour JSON. Au lieu de cela, vous travaillez avec des bibliothèques. `xml2js` en est un exemple. Il transforme l'XML en JSON, rendant les données plus faciles à manipuler pour les gourous de JavaScript.

## Voir aussi
- [MDN Web Docs sur XML](https://developer.mozilla.org/en-US/docs/Web/XML/XML_introduction)
- [Package npm xml2js](https://www.npmjs.com/package/xml2js)
- [Tutoriel XML de W3Schools](https://www.w3schools.com/xml/)
