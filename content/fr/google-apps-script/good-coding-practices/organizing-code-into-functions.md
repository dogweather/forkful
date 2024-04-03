---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 21:56:13.064109-07:00
description: "Comment faire : Dans Google Apps Script, qui est bas\xE9 sur JavaScript,\
  \ vous d\xE9finissez des fonctions en utilisant le mot-cl\xE9 `function`, suivi\
  \ d'un nom de\u2026"
lastmod: '2024-03-13T22:44:57.192341-06:00'
model: gpt-4-0125-preview
summary: "Dans Google Apps Script, qui est bas\xE9 sur JavaScript, vous d\xE9finissez\
  \ des fonctions en utilisant le mot-cl\xE9 `function`, suivi d'un nom de fonction\
  \ unique, de parenth\xE8ses `()` qui peuvent contenir des param\xE8tres, et d'accolades\
  \ `{}` qui encapsulent le bloc de code de la fonction."
title: Organiser le code en fonctions
weight: 18
---

## Comment faire :
Dans Google Apps Script, qui est basé sur JavaScript, vous définissez des fonctions en utilisant le mot-clé `function`, suivi d'un nom de fonction unique, de parenthèses `()` qui peuvent contenir des paramètres, et d'accolades `{}` qui encapsulent le bloc de code de la fonction. Voici un exemple basique :

```javascript
function greetUser() {
  var user = Session.getActiveUser().getEmail();
  Logger.log('Bonjour, ' + user + '!');
}

greetUser();
```

Exemple de sortie :

```
Bonjour, quelqu'un@example.com!
```

Considérons maintenant un exemple plus pratique lié à Google Sheets où nous séparons la fonctionnalité en deux fonctions : une pour configurer la feuille et une autre pour la remplir avec des données.

```javascript
function setupSheet() {
  var ss = SpreadsheetApp.getActiveSpreadsheet();
  var sheet = ss.getSheets()[0];
  sheet.setName('Données de Vente');
  sheet.appendRow(['Article', 'Quantité', 'Prix']);
}

function populateSheet(data) {
  var sheet = SpreadsheetApp.getActiveSpreadsheet().getSheetByName('Données de Vente');
  data.forEach(function(row) {
    sheet.appendRow(row);
  });
}

// Initialiser le tableau de données
var salesData = [
  ['Widgets', 15, 2.5],
  ['Gadgets', 8, 3.75]
];

// Exécuter les fonctions
setupSheet();
populateSheet(salesData);
```

Dans cet exemple, `setupSheet` prépare la feuille et `populateSheet` prend un tableau de données de vente pour remplir la feuille. Séparer ces préoccupations rend le code plus propre et plus adaptable aux changements.

## Approfondissement
Le concept de division du code en fonctions n'est pas nouveau ou unique à Google Apps Script ; c'est une pratique de programmation fondamentale préconisée dans presque tous les langages de programmation. Historiquement, les fonctions ont évolué à partir du concept mathématique de mappage des entrées aux sorties, qui est devenu une pierre angulaire de la programmation structurée. Cette approche favorise la modularité et la réutilisation du code, offrant des voies claires pour tester les parties individuelles du script.

Google Apps Script, étant basé sur JavaScript, bénéficie grandement des fonctions de première classe de JavaScript, permettant aux fonctions d'être passées en tant qu'arguments, retournées d'autres fonctions et attribuées à des variables. Cette fonctionnalité ouvre des modèles avancés comme les callbacks et la programmation fonctionnelle, bien que ces modèles puissent introduire une complexité qui pourrait être inutile pour des tâches d'automatisation simples dans Google Apps Script.

Pour des projets plus importants ou des applications plus complexes, les développeurs peuvent explorer l'utilisation des fonctionnalités plus récentes de JavaScript comme les fonctions fléchées, async/await pour les opérations asynchrones, et même TypeScript pour la typage statique. TypeScript, en particulier, peut être compilé pour s'exécuter en tant que Google Apps Script, offrant une avenue pour les développeurs qui recherchent un contrôle de type plus robuste et des fonctionnalités orientées objet avancées.

Cependant, pour la plupart des besoins de scriptage au sein de la suite Google Apps, s'en tenir à des fonctions simples et bien organisées comme démontré fournit une base solide. C'est toujours un équilibre entre exploiter des fonctionnalités avancées pour l'efficacité et maintenir la simplicité pour faciliter la maintenance et la lisibilité.
