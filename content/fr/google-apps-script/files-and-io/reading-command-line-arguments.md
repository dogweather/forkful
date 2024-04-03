---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 21:58:52.008390-07:00
description: "Lire les arguments de ligne de commande dans Google Apps Script est\
  \ un peu un abus de langage, car contrairement aux interfaces de ligne de commande\u2026"
lastmod: '2024-03-13T22:44:57.204728-06:00'
model: gpt-4-0125-preview
summary: Lire les arguments de ligne de commande dans Google Apps Script est un peu
  un abus de langage, car contrairement aux interfaces de ligne de commande traditionnelles
  dans des langages de programmation tels que Python ou Node.
title: Lecture des arguments de ligne de commande
weight: 23
---

## Comment faire :
Pour imiter le processus de lecture des arguments de ligne de commande dans Google Apps Script, en particulier pour les applications Web, vous pouvez utiliser les paramètres de chaîne de requête. Lorsqu'un utilisateur accède à l'URL de l'application Web, vous pouvez ajouter des arguments tels que `?name=John&age=30` et les analyser au sein de votre code Apps Script. Voici comment vous pourriez configurer cela :

```javascript
function doGet(e) {
  var params = e.parameter; // Récupère les paramètres de la chaîne de requête
  var name = params['name']; // Obtient le paramètre 'name'
  var age = params['age']; // Obtient le paramètre 'age'

  // Exemple de sortie :
  var output = "Nom : " + name + ", Âge : " + age;
  return HtmlService.createHtmlOutput(output);
}

// Exemple d'URL : https://script.google.com/macros/s/your_script_id/exec?name=John&age=30
```

Lorsque vous accédez à l'URL avec les paramètres spécifiés, le script produit quelque chose comme :

```
Nom : John, Âge : 30
```

Cette approche est essentielle pour créer des interactions personnalisées dans les applications Web ou pour contrôler de manière programmatique l'exécution des scripts.

## Approfondissement
Les arguments de ligne de commande, tels que compris dans le contexte des langages de programmation traditionnels, apportent les capacités pour les scripts et les applications de traiter les paramètres d'exécution, permettant ainsi des exécutions de code flexibles et dynamiques basées sur l'entrée des utilisateurs ou sur des processus automatisés. Google Apps Script, étant un langage de script basé sur le cloud pour le développement d'applications légères dans l'écosystème Google Workspace, n'opère pas nativement via une interface de ligne de commande. Au lieu de cela, son exécution est largement événementielle ou manuellement déclenchée à travers l'UI de Apps Script et Google Workspace, ou via des applications Web qui peuvent analyser les paramètres d'URL en tant qu'arguments de ligne de commande pseudo.

Étant donné cette différence architecturale, les programmeurs venant d'un contexte de langages riches en CLI pourraient avoir besoin d'ajuster leur approche lors de l'automatisation des tâches ou du développement d'applications dans Google Apps Script. Au lieu d'analyser les arguments de ligne de commande traditionnels, tirer parti de la fonctionnalité d'application Web de Google Apps Script ou même des fonctions personnalisées de Google Sheets pour le traitement interactif des données peut servir des fins similaires. Bien que cela puisse sembler être une limitation au premier abord, cela encourage le développement d'interfaces plus conviviales et d'applications Web accessibles, s'alignant sur l'objectif de Google Apps Script d'intégration et d'extension transparentes des applications Google Workspace.

Pour les scénarios où une émulation plus proche du comportement CLI est primordiale (par exemple, l'automatisation des tâches avec des paramètres dynamiques), les développeurs pourraient explorer l'utilisation de plateformes externes qui appellent des applications Web Google Apps Script, en passant des paramètres via des URL en tant que méthode "ligne de commande" de fortune. Cependant, pour les projets natifs Google Apps Script, adopter le modèle événementiel et centré sur l'UI de la plateforme conduit souvent à des solutions plus simples et plus maintenables.
