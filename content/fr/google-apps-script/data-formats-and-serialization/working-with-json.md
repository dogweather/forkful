---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 22:05:29.910164-07:00
description: "JSON, ou JavaScript Object Notation, est un format l\xE9ger pour stocker\
  \ et transporter des donn\xE9es, id\xE9al pour la communication serveur-client et\
  \ les\u2026"
lastmod: '2024-03-11T00:14:31.248037-06:00'
model: gpt-4-0125-preview
summary: "JSON, ou JavaScript Object Notation, est un format l\xE9ger pour stocker\
  \ et transporter des donn\xE9es, id\xE9al pour la communication serveur-client et\
  \ les\u2026"
title: Travailler avec JSON
---

{{< edit_this_page >}}

## Quoi & Pourquoi ?

JSON, ou JavaScript Object Notation, est un format léger pour stocker et transporter des données, idéal pour la communication serveur-client et les fichiers de configuration. Les programmeurs l'utilisent dans Google Apps Script pour un échange de données sans faille entre les services Google (comme Sheets, Docs, Drive) et des sources externes, en raison de sa structure lisible par l'homme et de son intégration facile dans les environnements basés sur JavaScript.

## Comment faire :

Dans Google Apps Script, manipuler du JSON est un processus simple, en grande partie grâce au support natif que JavaScript offre pour l'analyse et la mise en chaîne de JSON. Voici quelques opérations courantes :

**1. Analyser du JSON** : Supposons que nous récupérions une chaîne JSON d'un service web ; l'analyser en un objet JavaScript est essentiel pour la manipulation des données.

```javascript
var jsonString = '{"name": "Sample Project", "version": "1.0.0"}';
var obj = JSON.parse(jsonString);
Logger.log(obj.name); // Sortie: Sample Project
```

**2. Mettre en chaîne des objets JavaScript** : À l'inverse, convertir un objet JavaScript en une chaîne JSON est utile lorsque nous devons envoyer des données depuis Apps Script vers un service externe.

```javascript
var projectData = {
  name: "Sample Project",
  version: "1.0.0"
};
var jsonString = JSON.stringify(projectData);
Logger.log(jsonString); // Sortie: '{"name":"Sample Project","version":"1.0.0"}'
```

**3. Travailler avec des données complexes** :
Pour des structures de données plus complexes, comme des tableaux d'objets, le processus reste le même, mettant en évidence la flexibilité du JSON pour la représentation des données.

```javascript
var projects = [
  {name: "Project 1", version: "1.0"},
  {name: "Project 2", version: "2.0"}
];
var jsonString = JSON.stringify(projects);
Logger.log(jsonString); // Sortie: '[{"name":"Project 1","version":"1.0"},{"name":"Project 2","version":"2.0"}]'
```

## Plongée Profonde

L'omniprésence du JSON dans les applications web modernes ne peut être sous-estimée, ancrée dans sa simplicité et son intégration transparente avec JavaScript, le langage du web. Son design, inspiré par les littéraux d'objets JavaScript, bien que plus strict, facilite son adoption rapide. Au début des années 2000, le JSON a gagné en popularité comme une alternative à XML pour les applications web basées sur AJAX, offrant un format d'échange de données plus léger et moins verbeux. Étant donné l'intégration profonde de Google Apps Script avec diverses APIs Google et services externes, le JSON sert de format pivot pour structurer, transporter et manipuler des données à travers ces plateformes.

Bien que le JSON règne en maître pour les applications web, des formats de données alternatifs comme YAML pour les fichiers de configuration ou Protobuf pour une sérialisation binaire plus efficace dans des environnements à haute performance existent. Cependant, l'équilibre du JSON entre lisibilité, facilité d'utilisation et large support à travers les langages de programmation et outils solidifie sa position comme choix par défaut pour de nombreux développeurs se lançant dans Google Apps Script et au-delà.
