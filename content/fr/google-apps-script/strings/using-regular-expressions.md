---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 22:04:48.383720-07:00
description: "Les expressions r\xE9guli\xE8res (regex) sont des motifs utilis\xE9\
  s pour rechercher des combinaisons de caract\xE8res dans des cha\xEEnes de caract\xE8\
  res. Les\u2026"
lastmod: '2024-02-25T18:49:54.070285-07:00'
model: gpt-4-0125-preview
summary: "Les expressions r\xE9guli\xE8res (regex) sont des motifs utilis\xE9s pour\
  \ rechercher des combinaisons de caract\xE8res dans des cha\xEEnes de caract\xE8\
  res. Les\u2026"
title: "Utilisation des expressions r\xE9guli\xE8res"
---

{{< edit_this_page >}}

## Quoi & Pourquoi ?

Les expressions régulières (regex) sont des motifs utilisés pour rechercher des combinaisons de caractères dans des chaînes de caractères. Les programmeurs les utilisent pour rechercher, éditer ou manipuler du texte et des données, ce qui les rend indispensables pour les tâches de correspondance de motifs et d'analyse de données.

## Comment :

Utiliser des expressions régulières dans Google Apps Script est simple grâce à la syntaxe basée sur JavaScript. Voici comment vous pouvez intégrer regex dans vos scripts pour des tâches courantes telles que la recherche et la validation des données.

### Recherche dans les chaînes de caractères

Supposons que vous voulez trouver si une chaîne contient un motif spécifique, tel qu'une adresse e-mail. Voici un exemple simple :

```javascript
function findEmailInText(text) {
  var emailPattern = /\b[A-Za-z0-9._%+-]+@[A-Za-z0-9.-]+\.[A-Z|a-z]{2,}\b/;
  var found = text.match(emailPattern);
  if (found) {
    Logger.log("Trouvé : " + found[0]);
  } else {
    Logger.log("Aucun e-mail trouvé.");
  }
}

// Exemple d'utilisation
findEmailInText("Contactez-nous à info@example.com.");
```

### Validation des données

Les expressions régulières brillent dans la validation des données. Ci-dessous se trouve une fonction qui valide une chaîne de caractères pour vérifier si elle respecte une politique de mot de passe simple (au moins une lettre majuscule, une lettre minuscule et un minimum de 8 caractères).

```javascript
function validatePassword(password) {
  var passwordPattern = /^(?=.*[a-z])(?=.*[A-Z]).{8,}$/;
  return passwordPattern.test(password);
}

// Exemple de sortie
Logger.log(validatePassword("Str0ngPass")); // Renvoie : vrai
Logger.log(validatePassword("faible"));     // Renvoie : faux
```

## Plongée profonde

Les expressions régulières dans Google Apps Script sont héritées de JavaScript, standardisées pour la première fois dans la spécification du langage ECMAScript en juin 1997. Bien qu'elles soient puissantes, elles peuvent parfois conduire à un code déroutant et difficile à maintenir, surtout lorsqu'elles sont surutilisées ou utilisées pour des tâches de correspondance de motifs complexes qui pourraient être résolues plus efficacement par d'autres méthodes d'analyse.

Par exemple, bien que vous puissiez utiliser regex pour l'analyse de HTML ou de XML en dépannage, cela est généralement déconseillé en raison des structures imbriquées et complexes de ces documents. Au lieu de cela, des outils spécifiquement conçus pour analyser de telles structures, comme les analyseurs DOM pour HTML, sont plus fiables et lisibles.

De plus, les développeurs de Google Apps Script devraient être attentifs aux problèmes de performance potentiels lors de l'utilisation de motifs regex complexes dans des tâches de manipulation de texte à grande échelle, car le traitement regex peut être intensif en CPU. Dans de tels cas, diviser la tâche en sous-tâches plus simples ou utiliser des fonctions de manipulation de chaînes intégrées pourrait offrir un meilleur équilibre entre performance et maintenabilité.
