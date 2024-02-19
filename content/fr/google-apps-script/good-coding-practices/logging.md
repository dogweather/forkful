---
aliases:
- /fr/google-apps-script/logging/
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 21:57:02.633189-07:00
description: "La journalisation en programmation consiste \xE0 enregistrer des \xE9\
  v\xE9nements, des erreurs ou des occurrences notables pendant l'ex\xE9cution. Les\
  \ programmeurs le\u2026"
lastmod: 2024-02-18 23:09:08.296741
model: gpt-4-0125-preview
summary: "La journalisation en programmation consiste \xE0 enregistrer des \xE9v\xE9\
  nements, des erreurs ou des occurrences notables pendant l'ex\xE9cution. Les programmeurs\
  \ le\u2026"
title: Journalisation
---

{{< edit_this_page >}}

## Quoi & Pourquoi ?

La journalisation en programmation consiste à enregistrer des événements, des erreurs ou des occurrences notables pendant l'exécution. Les programmeurs le font pour déboguer des problèmes, surveiller les performances et conserver un enregistrement des données opérationnelles, ce qui est essentiel pour maintenir et comprendre le comportement du logiciel en production.

## Comment faire :

Dans Google Apps Script, la journalisation peut être effectuée en utilisant diverses méthodes, telles que la classe `Logger` et `console.log()`. La classe Logger est la manière traditionnelle, adaptée pour un débogage simple et des fins de développement. À partir des mises à jour récentes, `console.log()` offre plus de flexibilité et d'intégration avec Stackdriver Logging, fournissant une solution plus robuste pour surveiller vos Apps Scripts dans Google Cloud Platform.

**Utilisation de Logger :**

```javascript
function logSample() {
  Logger.log('Ceci est un message de log simple');
  
  var value = 5;
  Logger.log('La valeur est : %s', value); // Formatage de chaîne
}

// Pour voir le log :
// 1. Exécutez la fonction logSample.
// 2. Affichage -> Journaux
```

**Exemple de sortie Logger :**

```
[22-04-20 10:00:00:000 PDT] Ceci est un message de log simple
[22-04-20 10:00:00:001 PDT] La valeur est : 5
```

**Utilisation de console.log() :**

```javascript
function consoleLogSample() {
  console.log('Ce message est envoyé à Stackdriver Logging');
  const obj = {name: 'Jane', role: 'Développeur'};
  console.info('Journalisation d’un objet :', obj);
}

// Les logs peuvent être consultés dans la console Google Cloud Platform (GCP) sous Stackdriver Logging
```

**Exemple de sortie console.log() :**

```
Ce message est envoyé à Stackdriver Logging
Journalisation d’un objet : {name: "Jane", role: "Développeur"}
```

En passant à `console.log()` pour les applications complexes, les développeurs peuvent analyser efficacement les logs en utilisant les filtres et outils puissants fournis par GCP, ce qui n'est pas aussi direct avec la classe Logger traditionnelle.

## Approfondissement :

La journalisation dans Google Apps Script a évolué de manière significative. Initialement, la classe `Logger` était la méthode principale pour les développeurs pour déboguer leurs scripts. Elle est simple et suffisante pour les scripts basiques, mais elle manque de capacités nécessaires pour les applications cloud modernes, telles que la recherche dans les logs ou l'analyse des tendances des logs au fil du temps.

L'introduction de `console.log()` a comblé cette lacune en intégrant la journalisation Google Apps Script avec Stackdriver Logging de Google Cloud (maintenant appelé Operations Suite), fournissant une plateforme centralisée pour la journalisation, la surveillance et le débogage des applications. Ceci a non seulement permis la journalisation à grande échelle, mais a également ouvert des fonctionnalités avancées de gestion des logs, comme les métriques basées sur les logs, l'analyse des logs en temps réel et l'intégration avec d'autres services Google Cloud.

Bien que `Logger` serve toujours à des fins de débogage rapide et de journalisation dans des scripts plus petits, l'évolution vers l'utilisation de `console.log()` reflète un changement plus large dans le développement d'applications scalables et natives du cloud. Cela souligne l'engagement de Google à fournir aux développeurs des outils adaptés à la complexité et à l'échelle des applications d'aujourd'hui. Cependant, les nouveaux venus doivent être conscients de la courbe d'apprentissage un peu plus abrupte et de la nécessité de se familiariser avec les concepts de Google Cloud Platform. Malgré cela, le changement est avantageux pour les développeurs cherchant à tirer pleinement parti des capacités du cloud. Cet alignement avec les services cloud fait partie d'une tendance plus large dans le développement de logiciels, soulignant l'importance de mécanismes de journalisation robustes et scalables à l'ère du cloud computing.
