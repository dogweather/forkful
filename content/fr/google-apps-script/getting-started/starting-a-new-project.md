---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 22:02:29.077781-07:00
description: "D\xE9marrer un nouveau projet dans Google Apps Script (GAS) n\xE9cessite\
  \ d'initialiser un fichier de script au sein de l'\xE9cosyst\xE8me Google (Google\
  \ Drive, Docs,\u2026"
lastmod: '2024-03-11T00:14:31.223961-06:00'
model: gpt-4-0125-preview
summary: "D\xE9marrer un nouveau projet dans Google Apps Script (GAS) n\xE9cessite\
  \ d'initialiser un fichier de script au sein de l'\xE9cosyst\xE8me Google (Google\
  \ Drive, Docs,\u2026"
title: "D\xE9marrer un nouveau projet"
---

{{< edit_this_page >}}

## Quoi & Pourquoi ?

Démarrer un nouveau projet dans Google Apps Script (GAS) nécessite d'initialiser un fichier de script au sein de l'écosystème Google (Google Drive, Docs, Sheets, etc.) pour automatiser des tâches ou étendre les fonctionnalités des applications Google. Les programmeurs se lancent souvent dans cette aventure pour rationaliser les flux de travail, manipuler les services Google de manière programmatique ou créer des add-ons personnalisés, économisant ainsi du temps et exploitant la puissance de l'infrastructure de Google.

## Comment faire :

Pour commencer un nouveau projet dans Google Apps Script, vous avez plusieurs points d'entrée, mais concentrons-nous sur la méthode la plus directe : la création d'un script à partir de Google Drive.

1. **Créer un Projet dans Google Drive**
   - Naviguez vers Google Drive (drive.google.com).
   - Cliquez sur "+ Nouveau" > "Plus" > "Google Apps Script".
   - Un nouveau projet de script s'ouvre dans l'éditeur. Par défaut, il contient un fichier `Code.gs` avec un exemple de `myFunction`.

2. **Configuration de votre projet**
   - Renommez votre projet pour plus de clarté. Cliquez sur "Projet sans titre" en haut à gauche, et donnez-lui un nom significatif.
   - Écrivez une fonction simple dans le fichier `Code.gs` pour vous familiariser :

```javascript
function helloWorld() {
  Logger.log('Bonjour, le monde !');
}
```

   - Exécutez `helloWorld` en sélectionnant la fonction dans le menu déroulant à côté du bouton de lecture (▶) et en cliquant dessus. Cela exécutera la fonction.

3. **Voir les logs**
   - Pour voir le résultat de `Logger.log`, allez dans "Afficher" > "Logs", ou appuyez sur `Ctrl + Entrée`. Vous devriez voir "Bonjour, le monde !" dans les logs.

Félicitations, vous venez de commencer avec succès un nouveau projet dans Google Apps Script et exécuté une fonction simple !

## Plongée en Profondeur

L'introduction de Google Apps Script vers 2009 a fourni une plateforme puissante mais accessible tant pour les développeurs que pour les non-développeurs pour automatiser, étendre et construire sur le vaste array de services Google. Contrairement aux environnements de programmation traditionnels, GAS offre un mélange unique de simplicité et d'intégration, directement au sein de l'écosystème Google, sans besoin de serveurs externes ni de configuration. Ce modèle d'exécution sans serveur simplifie grandement le déploiement et la gestion des projets.

Historiquement, GAS était quelque peu limité par son environnement d'exécution et sa version de langue, souvent en retard par rapport aux standards actuels de JavaScript. Cependant, des mises à jour récentes ont apporté la syntaxe moderne de JavaScript (ECMAScript 2015+) à GAS, le rendant plus appétissant pour les développeurs habitués aux pratiques de développement contemporaines.

Bien que GAS soit positionné de manière unique pour interagir avec les services Google, il existe des approches alternatives pour les besoins plus intensifs ou spécifiques. Par exemple, Google Cloud Functions et Google Cloud Platform (GCP) offrent des solutions plus robustes et évolutives pour gérer des flux de travail complexes, traiter de grands ensembles de données et s'intégrer avec des API externes. Ces plateformes permettent de programmer dans divers langages (par ex., Python, Go, Node.js) et offrent des ressources informatiques plus importantes.

Néanmoins, pour les tâches étroitement liées aux applications Google, à l'automatisation et au développement rapide au sein de cet écosystème, Google Apps Script reste un outil inégalé en termes de facilité d'utilisation et de profondeur d'intégration. Son accessibilité directement depuis Google Drive et sa connexion transparente aux services Google en font un choix pratique pour un large éventail de projets, en particulier pour ceux qui cherchent à étendre la fonctionnalité de Sheets, Docs, Forms et autres applications Google.
