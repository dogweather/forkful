---
date: 2024-01-26 04:42:03.153866-07:00
description: "Les nombres complexes sont des nombres ayant une partie r\xE9elle et\
  \ une partie imaginaire (comme 3 + 4i). Ils apparaissent dans divers probl\xE8mes\
  \ de\u2026"
lastmod: '2024-03-13T22:44:58.268289-06:00'
model: gpt-4-0125-preview
summary: "Les nombres complexes sont des nombres ayant une partie r\xE9elle et une\
  \ partie imaginaire (comme 3 + 4i)."
title: Manipulation des nombres complexes
weight: 14
---

## Comment faire :
JavaScript ne possède pas de support intégré pour les nombres complexes, mais vous pouvez retrousser vos manches et vous en charger avec des objets et des mathématiques. Voici une rapide introduction.

```javascript
class NombreComplexe {
  constructor(reel, imaginaire) {
    this.reel = reel;
    this.imaginaire = imaginaire;
  }

  add(autre) {
    return new NombreComplexe(this.reel + autre.reel, this.imaginaire + autre.imaginaire);
  }

  // ...ajoutez d'autres méthodes (soustraire, multiplier, diviser) selon le besoin

  toString() {
    return `${this.reel} + ${this.imaginaire}i`;
  }
}

const a = new NombreComplexe(1, 2);
const b = new NombreComplexe(3, 4);
const result = a.add(b);

console.log(`Résultat : ${result}`); // Résultat : 4 + 6i
```

## Exploration Approfondie
Les nombres complexes existent depuis le 16e siècle, grâce au mathématicien italien Gerolamo Cardano. Ils sont devenus cruciaux dans divers domaines, comme l'ingénierie et la physique. Dans la programmation moderne, ils sont clés pour les simulations et les algorithmes nécessitant une multidimensionnalité.

Maintenant, JavaScript n'est pas nativement équipé pour les nombres complexes. Mais à part l'option de bricolage, vous pourriez utiliser des bibliothèques mathématiques comme math.js ou numeric.js. Elles offrent la puissance nécessaire pour manipuler les nombres complexes, ajoutant des avantages comme plus d'opérations, le calcul de la magnitude, et la recherche de l'argument.

Sous le capot, lorsque vous opérez avec des nombres complexes, c'est comme si vous gériez deux nombres séparés liés à la hanche. L'addition et la soustraction sont simples - associez le réel au réel, l'imaginaire à l'imaginaire. La multiplication et la division deviennent plus piquantes avec des danses de termes croisés et nécessitent plus d'attention.

## Voir Aussi
- MDN Web Docs sur JavaScript : https://developer.mozilla.org/en-US/docs/Web/JavaScript/A_re-introduction_to_JavaScript
- Math.js, une bibliothèque mathématique incluant les nombres complexes : https://mathjs.org/docs/datatypes/complex_numbers.html
- Numeric.js, une autre bibliothèque : http://numericjs.com/documentation.html
- Une exploration approfondie sur les nombres complexes (axée sur les maths) : https://mathworld.wolfram.com/ComplexNumber.html
