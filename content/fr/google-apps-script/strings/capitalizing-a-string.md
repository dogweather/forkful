---
title:                "Mettre une chaîne en majuscules"
aliases:
- /fr/google-apps-script/capitalizing-a-string/
date:                  2024-02-01T21:48:55.485052-07:00
model:                 gpt-4-0125-preview
simple_title:         "Mettre une chaîne en majuscules"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/google-apps-script/capitalizing-a-string.md"
changelog:
  - 2024-02-01, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Quoi et pourquoi ?

Mettre en majuscule une chaîne consiste à modifier l'entrée de manière que le premier caractère soit en majuscule tandis que les suivants restent en minuscule, communément utilisé pour formater des noms ou des titres. Les programmeurs font cela pour assurer la cohérence des données et améliorer la lisibilité au sein des interfaces utilisateur ou des documents.

## Comment faire :

Google Apps Script, étant basé sur JavaScript, permet plusieurs méthodes pour mettre en majuscule une chaîne, bien qu'il n'existe pas de fonction intégrée. Voici quelques exemples succincts :

**Méthode 1 : Utiliser charAt() et slice()**

```javascript
function capitalizeString(inputString) {
  if (!inputString) return '';
  return inputString.charAt(0).toUpperCase() + inputString.slice(1).toLowerCase();
}

// Exemple d'utilisation
let result = capitalizeString('bonjour, le monde');
console.log(result);  // Sortie : Bonjour, le monde
```

**Méthode 2 : Utiliser une Regex**

Pour ceux qui préfèrent une solution basée sur les expressions régulières pour gérer plus élégamment les cas particuliers :

```javascript
function capitalizeStringRegex(inputString) {
  return inputString.toLowerCase().replace(/^\w/, c => c.toUpperCase());
}

// Exemple d'utilisation
let result = capitalizeStringRegex('bonjour, le monde');
console.log(result);  // Sortie : Bonjour, le monde
```

Les deux méthodes garantissent que le premier caractère de la chaîne est en majuscule, et les suivants sont en minuscule, adaptées à une variété d'applications, y compris, mais sans s'y limiter, la manipulation de Google Sheets ou l'édition de documents via Apps Script.

## Approfondissement

Mettre des chaînes en majuscule dans Google Apps Script est simple, en exploitant les puissantes capacités de manipulation de chaînes de JavaScript. Historiquement, des langues comme Python offrent des méthodes intégrées telles que `.capitalize()` pour réaliser cela, ajoutant une étape supplémentaire mineure pour les programmeurs JavaScript et Apps Script. Cependant, l'absence de fonction intégrée dans JavaScript/Google Apps Script encourage la flexibilité et une compréhension approfondie des techniques de manipulation de chaînes.

Pour des scénarios complexes, comme la mise en majuscule de chaque mot d'une chaîne (Cas Titre), les programmeurs pourraient combiner les méthodes regex avec les fonctions `split()` et `map()` pour traiter chaque mot individuellement. Bien que Google Apps Script ne propose pas de méthode directe pour la mise en majuscule de chaînes, l'utilisation des méthodes de manipulation de chaînes de JavaScript existantes offre une grande flexibilité, permettant aux développeurs de gérer efficacement les chaînes selon leurs besoins spécifiques.

Dans les cas où la performance et l'efficacité sont primordiales, il est bon de noter que la manipulation directe de chaînes pourrait être plus performante que regex, en particulier pour les chaînes plus longues ou les opérations dans de grandes boucles. Cependant, pour la plupart des applications pratiques au sein de Google Apps Script, les deux approches fournissent des solutions fiables.
