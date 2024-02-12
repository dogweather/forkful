---
title:                "Écrire sur l'erreur standard"
aliases:
- /fr/google-apps-script/writing-to-standard-error.md
date:                  2024-02-01T22:08:48.869090-07:00
model:                 gpt-4-0125-preview
simple_title:         "Écrire sur l'erreur standard"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/google-apps-script/writing-to-standard-error.md"
changelog:
  - 2024-02-01, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Quoi & Pourquoi ?

Écrire sur l'erreur standard (stderr) dans les langues de programmation consiste à diriger les messages d'erreur et les diagnostics vers un flux séparé, distinct de la sortie standard (stdout). Les programmeurs font cela pour distinguer la sortie normale du programme des messages d'erreur, rendant le débogage et l'analyse des logs plus simples.

## Comment faire :

Google Apps Script, étant un langage de script pour le développement d'applications légères sur la plateforme Google Apps, ne fournit pas de fonction intégrée directe comme `console.error()` pour écrire sur stderr, comme vous pourriez le trouver dans Node.js ou Python. Cependant, vous pouvez simuler ce comportement en utilisant les services de journalisation de Google Apps Script ou une gestion personnalisée des erreurs pour gérer et séparer les sorties d'erreur.

### Exemple : Utilisation de `Logger` pour les Messages d'Erreur

```javascript
function logError() {
  try {
    // Simuler une erreur
    const result = 1 / 0;
    if(!isFinite(result)) throw new Error("Tentative de division par zéro");
  } catch (e) {
    // Écrire le message d'erreur dans les Logs
    Logger.log('Erreur : ' + e.message);
  }
}
```

Lorsque vous exécutez `logError()`, cela écrira le message d'erreur dans le journal de Google Apps Script, que vous pouvez voir en allant sur `Afficher > Journaux`. Ce n'est pas exactement stderr, mais cela sert un but similaire de séparer les journaux d'erreur des sorties standard.

### Journalisation Diagnostique Avancée

Pour un débogage et une journalisation d'erreurs plus avancés, vous pouvez utiliser la Journalisation Stackdriver, désormais connue sous le nom de Suite des Opérations de Google Cloud.

```javascript
function advancedErrorLogging() {
  try {
    // Provoquer une erreur délibérément
    const obj = null;
    const result = obj.someProperty;
  } catch (e) {
    console.error('Erreur rencontrée : ', e.toString());
  }
}
```

Cela dirigera le message d'erreur vers la Journalisation Stackdriver, où il est géré comme un journal de niveau erreur. Notez que l'intégration de Stackdriver/Suite des Opérations de Google Cloud offre une solution de journalisation plus granulaire et recherchable par rapport à `Logger`.

## Exploration Approfondie

L'absence d'un flux `stderr` dédié dans Google Apps Script reflète sa nature et ses origines en tant que langage de script basé sur le cloud, où les sorties traditionnelles de console ou de terminal (comme stdout et stderr) sont moins pertinentes. Historiquement, Google Apps Script était conçu pour améliorer la fonctionnalité des applications Google avec des scripts simples, en mettant l'accent sur la facilité d'utilisation plutôt que sur des fonctionnalités complètes disponibles dans des environnements de programmation plus complexes.

Cela dit, l'évolution de Google Apps Script vers le développement d'applications plus sophistiquées a incité les développeurs à adopter des approches créatives pour la gestion des erreurs et la journalisation, en utilisant les services disponibles comme Logger et en s'intégrant à la Suite des Opérations de Google Cloud. Ces méthodes, bien qu'elles ne soient pas des implémentations directes de stderr, offrent des alternatives robustes pour la gestion des erreurs et la journalisation diagnostique dans un environnement centré sur le cloud.

Critiquement, bien que ces méthodes servent le but dans l'écosystème de Google Apps Script, elles soulignent les limitations de la plateforme par rapport aux environnements de programmation traditionnels. Pour les développeurs nécessitant des stratégies de gestion des erreurs détaillées et hiérarchiques, l'intégration avec des services de journalisation externes ou l'adoption de Google Cloud Functions, qui offrent une gestion plus conventionnelle des erreurs stderr et stdout, pourrait être préférable.
