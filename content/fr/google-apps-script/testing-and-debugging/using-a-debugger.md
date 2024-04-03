---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 22:03:28.331482-07:00
description: "Google Apps Script propose un d\xE9bogueur int\xE9gr\xE9 dans l'\xE9\
  diteur Apps Script pour aider \xE0 d\xE9panner les scripts. Voici comment initier\
  \ et utiliser le\u2026"
lastmod: '2024-03-13T22:44:57.191275-06:00'
model: gpt-4-0125-preview
summary: "Google Apps Script propose un d\xE9bogueur int\xE9gr\xE9 dans l'\xE9diteur\
  \ Apps Script pour aider \xE0 d\xE9panner les scripts."
title: "Utiliser un d\xE9bogueur"
weight: 35
---

## Comment faire :
Google Apps Script propose un débogueur intégré dans l'éditeur Apps Script pour aider à dépanner les scripts. Voici comment initier et utiliser le débogueur :

1. **Ouvrez votre script dans l’éditeur Apps Script.**
2. **Sélectionnez une fonction à déboguer.** À partir du menu déroulant en haut, sélectionnez la fonction que vous souhaitez déboguer.
3. **Définissez des points d'arrêt.** Cliquez sur la gouttière (la zone grise à gauche des numéros de ligne) où vous souhaitez interrompre l'exécution ; un point rouge apparaît, indiquant un point d'arrêt.
4. **Commencez le débogage.** Cliquez sur l’icône du bug ou sélectionnez `Déboguer` > `Démarrer le débogage`. L'exécution commencera et s'arrêtera au premier point d'arrêt.

Considérez ce script simple :

```javascript
function calculerSomme() {
  var a = 5;
  var b = 10;
  var somme = a + b;
  Logger.log(somme); // Destiné à enregistrer 15
}
```

Si vous n'êtes pas sûr de la raison pour laquelle `Logger.log(somme)` n'affiche pas le résultat attendu, vous pourriez définir un point d'arrêt à la ligne `var somme = a + b;` et passer à travers le script ligne par ligne pour inspecter les valeurs des variables.

**Exemple de sortie dans Logger :**

```plain
15
```

Pendant le débogage, l'éditeur Apps Script vous permet de :

- **Passer à travers le code** en utilisant les boutons de pas à pas, d'entrer dans et de sortir de.
- **Observer les expressions et les variables** pour voir leurs valeurs changer en temps réel.
- **Inspecter la pile d'appels** pour tracer les appels de fonctions.

## Examen approfondi
Déboguer dans Google Apps Script, comme dans tout autre environnement de programmation, est essentiel pour créer des applications sans erreur. Introduit dès le début du développement de GAS, le débogueur intégré offre des capacités fondamentales pour inspecter et corriger le code progressivement. Bien qu’il fournisse des fonctionnalités de débogage de base semblables à celles trouvées dans des environnements plus matures comme Visual Studio Code ou IntelliJ, il peut être limité pour des scénarios de débogage complexes. Par exemple, ses capacités à inspecter les appels asynchrones ou à gérer l'exécution de scripts lourds pourraient être limitantes.

Pour des besoins de débogage complexes, les développeurs pourraient recourir à des méthodes alternatives telles que l'utilisation extensive de journaux (avec `Logger.log()`) ou même le déploiement en tant qu'application web pour inspecter le comportement dans un scénario réel. Cependant, la simplicité et l'intégration du débogueur de GAS au sein de l'éditeur Apps Script en font une première étape inestimable pour le dépannage et la compréhension du comportement du script. Notamment, avec les mises à jour et les améliorations continues de Google à Apps Script, l'expérience de débogage s'améliore régulièrement, offrant des outils et des options plus sophistiqués au fil du temps. Cette évolution reflète l'engagement de Google à faire d'Apps Script une plateforme plus puissante et accessible pour les développeurs de divers horizons.
