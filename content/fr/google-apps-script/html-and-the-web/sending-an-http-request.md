---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 22:01:27.545091-07:00
description: "Comment faire : Dans Google Apps Script, le moyen principal d'envoyer\
  \ une requ\xEAte HTTP est d'utiliser le service `UrlFetchApp`. Ce service fournit\
  \ des\u2026"
lastmod: '2024-03-13T22:44:57.181838-06:00'
model: gpt-4-0125-preview
summary: "Dans Google Apps Script, le moyen principal d'envoyer une requ\xEAte HTTP\
  \ est d'utiliser le service `UrlFetchApp`."
title: "Envoi d'une requ\xEAte HTTP"
weight: 44
---

## Comment faire :
Dans Google Apps Script, le moyen principal d'envoyer une requête HTTP est d'utiliser le service `UrlFetchApp`. Ce service fournit des méthodes pour faire des requêtes HTTP GET et POST. Voici un exemple simple de réalisation d'une requête GET pour récupérer des données JSON :

```javascript
function fetchJsonData() {
  var url = 'https://api.example.com/data';
  var response = UrlFetchApp.fetch(url);
  var json = response.getContentText();
  var data = JSON.parse(json);
  
  Logger.log(data);
}
```

Pour une requête POST, qui est communément utilisée pour envoyer des données à un serveur, vous devez inclure plus de détails dans le paramètre d'options :

```javascript
function postExample() {
  var url = 'https://api.example.com/post';
  var payload = {
    key1: 'value1',
    key2: 'value2'
  };
  
  var options = {
    'method' : 'post',
    'contentType': 'application/json',
    // Convertir l'objet JavaScript en une chaîne JSON
    'payload' : JSON.stringify(payload)
  };
  
  var response = UrlFetchApp.fetch(url, options);
  Logger.log(response.getContentText());
}
```

Ces extraits montrent des implémentations de base pour les requêtes GET et POST. Le résultat dépendra de la réponse de l'API et peut être consulté dans le Logger de Google Apps Script.

## Plongée profonde
Le service `UrlFetchApp` de Google Apps Script a évolué de manière significative depuis sa création, offrant un contrôle plus nuancé sur les requêtes HTTP avec des fonctionnalités telles que la définition des en-têtes, payload, et la gestion du multipart/form-data pour l'upload de fichiers. Bien qu'il offre un moyen simple d'intégrer des services web externes, les développeurs venant de langages backend plus robustes peuvent trouver ses fonctionnalités quelque peu limitées comparées aux bibliothèques comme `requests` en Python ou l'API `fetch` en JavaScript avec Node.js.

Une limitation notable est le temps d'exécution limité pour Google Apps Script, qui affecte les requêtes de longue durée. De plus, bien que `UrlFetchApp` couvre une large gamme de cas d'utilisation, les scénarios plus complexes impliquant l'authentification OAuth ou la gestion de très grands payloads peuvent nécessiter des solutions créatives ou l'exploitation de ressources Google Cloud supplémentaires.

Néanmoins, pour la plupart des intégrations que les développeurs de Google Workspace rencontrent— allant de l'automatisation de la récupération de données à la publication de mises à jour vers des services externes — `UrlFetchApp` fournit un outil puissant et accessible. Son intégration dans Google Apps Script signifie qu'il n'y a pas besoin de bibliothèques externes ou d'une configuration complexe, rendant l'exécution des requêtes HTTP relativement simple dans les contraintes de Google Apps Script. À mesure que le paysage des API web continue de s'étendre, `UrlFetchApp` reste un pont critique pour les programmes Google Apps Script interagir avec le monde au-delà de l'écosystème de Google.
