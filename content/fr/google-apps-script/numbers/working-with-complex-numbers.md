---
title:                "Travailler avec des nombres complexes"
date:                  2024-02-01T22:07:38.530232-07:00
model:                 gpt-4-0125-preview
simple_title:         "Travailler avec des nombres complexes"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/google-apps-script/working-with-complex-numbers.md"
changelog:
  - 2024-02-01, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Quoi & Pourquoi ?
Les nombres complexes, représentés comme une combinaison d'unités réelles et imaginaires (par exemple, 3 + 4i), sont fondamentaux dans divers problèmes de calcul, notamment en ingénierie, en physique et en mathématiques appliquées. Apprendre à manipuler ces nombres dans Google Apps Script permet aux programmeurs d'étendre leurs capacités au calcul scientifique, au traitement du signal, et au-delà.

## Comment faire :
Google Apps Script ne dispose pas d'un support intégré pour les nombres complexes, nécessitant l'implémentation d'une fonctionnalité personnalisée. Ci-dessous se trouve une structure de base pour gérer les nombres complexes, y compris l'addition, la soustraction, et la multiplication.

```javascript
// Définir un constructeur pour les nombres complexes
function Complex(reel, imag) {
  this.reel = reel;
  this.imag = imag;
}

// Méthode pour ajouter deux nombres complexes
Complex.prototype.add = function(autre) {
  return new Complex(this.reel + autre.reel, this.imag + autre.imag);
};

// Méthode pour soustraire deux nombres complexes
Complex.prototype.subtract = function(autre) {
  return new Complex(this.reel - autre.reel, this.imag - autre.imag);
};

// Méthode pour multiplier deux nombres complexes
Complex.prototype.multiply = function(autre) {
  return new Complex(
    this.reel * autre.reel - this.imag * autre.imag,
    this.reel * autre.imag + this.imag * autre.reel
  );
};

// Exemple d'utilisation
var num1 = new Complex(3, 4);
var num2 = new Complex(1, 2);

// Ajouter deux nombres complexes
var somme = num1.add(num2);
console.log(`Somme: ${somme.reel} + ${somme.imag}i`); // Somme: 4 + 6i

// Soustraire deux nombres complexes
var difference = num1.subtract(num2);
console.log(`Différence: ${difference.reel} + ${difference.imag}i`); // Différence: 2 + 2i

// Multiplier deux nombres complexes
var produit = num1.multiply(num2);
console.log(`Produit: ${produit.reel} + ${produit.imag}i`); // Produit: -5 + 10i
```

## Plongée Profonde :
Le concept des nombres complexes remonte au 16ème siècle, mais c'était le travail de mathématiciens comme Euler et Gauss qui a solidifié leur place en mathématiques. Malgré leur utilité, les nombres complexes ne sont pas directement pris en charge en JavaScript ou, par extension, dans Google Apps Script. Le manque de support natif signifie que les opérations sur les nombres complexes doivent être mises en œuvre manuellement, comme démontré. Bien que cela offre une bonne opportunité d'apprentissage et une fonctionnalité suffisante pour les besoins de base, pour des travaux de calcul lourds nécessitant des nombres complexes, on pourrait envisager d'utiliser d'autres environnements de programmation mieux adaptés au calcul mathématique, tels que Python avec NumPy, qui offrent des opérations intégrées et hautement optimisées pour gérer les nombres complexes. Néanmoins, comprendre et mettre en œuvre les opérations de base dans Google Apps Script est un exercice utile pour ceux qui cherchent à élargir leurs compétences en programmation et à les appliquer dans un large éventail de contextes.
