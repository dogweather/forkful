---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 22:00:43.642408-07:00
description: "Arrondir les nombres, un concept fondamental en programmation informatique,\
  \ consiste \xE0 ajuster un nombre \xE0 l'entier le plus proche ou \xE0 un nombre\
  \ sp\xE9cifi\xE9\u2026"
lastmod: 2024-02-19 22:05:16.086220
model: gpt-4-0125-preview
summary: "Arrondir les nombres, un concept fondamental en programmation informatique,\
  \ consiste \xE0 ajuster un nombre \xE0 l'entier le plus proche ou \xE0 un nombre\
  \ sp\xE9cifi\xE9\u2026"
title: Arrondissement des nombres
---

{{< edit_this_page >}}

## Quoi et Pourquoi ?

Arrondir les nombres, un concept fondamental en programmation informatique, consiste à ajuster un nombre à l'entier le plus proche ou à un nombre spécifié de décimales. Les programmeurs effectuent souvent des arrondis pour simplifier les nombres pour la lisibilité humaine ou pour répondre à des besoins de calcul spécifiques, assurant ainsi précision et réduisant la charge de calcul.

## Comment faire :

Google Apps Script, étant un langage basé sur JavaScript, offre des méthodes standard pour arrondir les nombres. Voici un aperçu de trois techniques couramment utilisées :

### Math.round()
Cette fonction arrondit un nombre à l'entier le plus proche.

```javascript
var number = 2.56;
var roundedNumber = Math.round(number); 
Logger.log(roundedNumber); // Affiche : 3
```

### Math.ceil()
Arrondit un nombre à l'entier supérieur le plus proche.

```javascript
var number = 2.56;
var roundedUp = Math.ceil(number); 
Logger.log(roundedUp); // Affiche : 3
```

### Math.floor()
Au contraire, arrondit un nombre à l'entier inférieur le plus proche.

```javascript
var number = 2.56;
var roundedDown = Math.floor(number); 
Logger.log(roundedDown); // Affiche : 2
```

Pour des décimales spécifiques, vous pouvez utiliser `.toFixed()`, qui renvoie en fait une chaîne de caractères, ou une approche plus nuancée pour un arrondi mathématique :

```javascript
var number = 2.56789;
var fixedNumber = number.toFixed(2); 
Logger.log(fixedNumber); // Affiche : "2.57" (en tant que chaîne)

var preciseRound = Math.round(number * 100) / 100; 
Logger.log(preciseRound); // Affiche : 2.57
```

## Approfondissement

Arrondir les nombres dans Google Apps Script ne diffère pas beaucoup de la façon dont cela se fait dans d'autres environnements JavaScript. Cependant, comprendre les différences dans les méthodes d'arrondi et le potentiel de problèmes d'arithmétique à virgule flottante est crucial. Par exemple, en raison de la manière dont les ordinateurs représentent les nombres à virgule flottante, toutes les fractions décimales ne peuvent pas être représentées avec une précision parfaite, conduisant parfois à des résultats d'arrondi inattendus.

Historiquement, JavaScript (et par extension, Google Apps Script) gère cela en se conformant à la norme IEEE 754, utilisée par de nombreux autres langages de programmation pour l'arithmétique à virgule flottante. Cette norme définit comment les nombres sont arrondis, assurant une cohérence entre diverses plateformes et langages.

Bien que les méthodes d'arrondi direct dans Google Apps Script soient simples et souvent suffisantes, les applications complexes ou de haute précision pourraient bénéficier des bibliothèques comme decimal.js ou big.js, qui sont conçues pour gérer l'arithmétique de précision arbitraire. Celles-ci peuvent être particulièrement utiles lorsqu'on travaille avec des calculs financiers ou scientifiques où la précision des nombres arrondis est primordiale.

Rappelez-vous, cependant, que l'utilisation de bibliothèques externes dans Google Apps Script nécessite de les charger via l'éditeur de script, ce qui peut introduire des dépendances ou affecter la performance de votre script selon son utilisation. Dans de nombreux cas, les méthodes Math intégrées sont entièrement adéquates, mais pour ces cas limites qui nécessitent une précision au plus haut degré, regarder au-delà de la bibliothèque standard peut être nécessaire.
