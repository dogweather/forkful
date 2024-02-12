---
title:                "Génération de nombres aléatoires"
aliases: - /fr/google-apps-script/generating-random-numbers.md
date:                  2024-02-01T21:53:47.167664-07:00
model:                 gpt-4-0125-preview
simple_title:         "Génération de nombres aléatoires"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/google-apps-script/generating-random-numbers.md"
changelog:
  - 2024-02-01, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Quoi & Pourquoi ?

La génération de nombres aléatoires est une tâche fondamentale en programmation utilisée pour une multitude d'applications, telles que les simulations, les jeux et les systèmes de sécurité. Les programmeurs utilisent cette technique dans Google Apps Script pour introduire de la variabilité, tester des scénarios et ajouter de l'imprévisibilité à leurs applications au sein de l'écosystème Google, y compris Sheets, Docs et Forms.

## Comment faire :

Dans Google Apps Script, vous pouvez générer des nombres aléatoires à l'aide de la fonction `Math.random()`, similaire à JavaScript. Cette fonction retourne un nombre pseudo-aléatoire à virgule flottante dans la plage de 0 (inclus) à 1 (exclus). Pour adapter ces nombres à divers cas d'utilisation, tels que la génération d'entiers dans une plage spécifique, vous devrez peut-être effectuer des calculs supplémentaires.

### Générer un Nombre Aléatoire Basique

Pour générer un nombre aléatoire simple et l'enregistrer dans la console :

```javascript
function generateRandomNumber() {
  var randomNumber = Math.random();
  Logger.log(randomNumber);
}
```
*Exemple de sortie :* `0.1234567890123456`

### Générer un Entier dans une Plage Spécifique

Pour générer un entier aléatoire entre deux valeurs (`min` et `max`), incluses :

```javascript
function getRandomInt(min, max) {
  min = Math.ceil(min);
  max = Math.floor(max);
  var randomNumber = Math.floor(Math.random() * (max - min + 1)) + min;
  Logger.log(randomNumber);
  return randomNumber;
}

// Exemple :
getRandomInt(1, 10);
```
*Exemple de sortie :* `7`

Rappelez-vous, la fonction `Math.ceil()` est utilisée pour arrondir la valeur minimale à l'entier supérieur et `Math.floor()` pour arrondir la valeur maximale à l'entier inférieur, assurant ainsi que le nombre aléatoire se situe dans la plage spécifiée.

## Plongée Profonde

Le mécanisme de génération de nombres aléatoires dans Google Apps Script, et en effet dans la plupart des langages de programmation, utilise un générateur de nombres pseudo-aléatoires (PRNG). Cette technique est déterministe et repose sur une valeur initiale, connue sous le nom de graine, pour produire une séquence de nombres qui semble aléatoire. Bien que suffisante pour de nombreuses applications, il est important de noter que les nombres pseudo-aléatoires peuvent ne pas convenir lorsque une haute sécurité ou une vraie aléatoireité est requise, comme dans les applications cryptographiques.

La vraie aléatoireité peut être obtenue grâce à des générateurs de nombres aléatoires matériels ou à des services qui génèrent de l'aléatoireité à partir de phénomènes naturels. Cependant, pour la plupart des besoins de script quotidien dans Google Apps Script, `Math.random()` suffit.

Historiquement, la quête de techniques de génération de nombres aléatoires plus efficaces a conduit au développement de divers algorithmes, avec des exemples notables étant le Mersenne Twister et le générateur linéaire congruentiel (LCG). Cependant, étant donné le haut niveau d'abstraction dans Google Apps Script, la plupart des utilisateurs n'auront pas besoin d'implémenter ces algorithmes directement, mais comprendre les principes sous-jacents peut aider à apprécier l'importance et les limitations de la génération de nombres aléatoires dans vos scripts.
