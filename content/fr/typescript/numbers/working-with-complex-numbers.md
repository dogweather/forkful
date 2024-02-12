---
title:                "Manipulation des nombres complexes"
aliases:
- /fr/typescript/working-with-complex-numbers.md
date:                  2024-01-26T04:46:20.814984-07:00
model:                 gpt-4-0125-preview
simple_title:         "Manipulation des nombres complexes"

tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/typescript/working-with-complex-numbers.md"
---

{{< edit_this_page >}}

## Quoi et pourquoi ?
Les nombres complexes, composés d'une partie réelle et d'une partie imaginaire (généralement écrits sous la forme a + bi), rendent possibles des calculs impraticables ou impossibles avec de simples nombres réels. Les programmeurs les utilisent dans des domaines tels que le traitement du signal, l'informatique quantique et les mathématiques appliquées, où les représentations numériques bidimensionnelles sont essentielles.

## Comment :
Manipuler des nombres complexes en TypeScript nécessite une classe dédiée. Créons-en une et travaillons sur l'addition et la multiplication.

```TypeScript
class Complex {
    constructor(public re: number, public im: number) {}

    add(other: Complex): Complex {
        return new Complex(this.re + other.re, this.im + other.im);
    }

    multiply(other: Complex): Complex {
        return new Complex(
            this.re * other.re - this.im * other.im,
            this.re * other.im + this.im * other.re
        );
    }

    toString(): string {
        return `${this.re} + ${this.im}i`;
    }
}

let num1 = new Complex(1, 2);
let num2 = new Complex(3, 4);
let sum = num1.add(num2);
let product = num1.multiply(num2);

console.log(`Somme : ${sum.toString()}`); // Sortie : Somme : 4 + 6i
console.log(`Produit : ${product.toString()}`); // Sortie : Produit : -5 + 10i
```

## Plongée profonde
Historiquement, les nombres complexes étaient controversés - même qualifiés d'"imaginaires" pour exprimer le scepticisme initial. Aujourd'hui, ils sont fondamentaux dans les mathématiques et la science modernes.

Les alternatives à notre classe simple pourraient impliquer l'utilisation de bibliothèques existantes telles que `math.js` ou `complex.js`, détaillées avec des fonctionnalités supplémentaires comme les fonctions trigonométriques, l'exponentiation et la conjugaison complexe.

Les détails de notre mise en œuvre TypeScript se résument à définir des opérations arithmétiques. La méthode `add` ajoute simplement les parties correspondantes. `multiply` applique la méthode FOIL utilisée en algèbre, en se souvenant que `i^2 = -1`.

## Voir aussi
Pour plus de lectures et de ressources sur les nombres complexes et leur utilisation en programmation, consultez :

- Algèbre de nombres complexes sur MDN : https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/BigInt
- Bibliothèque `math.js` : https://mathjs.org/docs/datatypes/complex_numbers.html
- Bibliothèque `complex.js` : https://complex-js.github.io/complex.js/
