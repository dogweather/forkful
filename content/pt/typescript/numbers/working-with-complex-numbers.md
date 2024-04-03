---
date: 2024-01-26 04:46:22.655713-07:00
description: "Como fazer: Manusear n\xFAmeros complexos em TypeScript requer uma classe\
  \ dedicada. Vamos criar uma e trabalhar com adi\xE7\xE3o e multiplica\xE7\xE3o."
lastmod: '2024-03-13T22:44:46.319856-06:00'
model: gpt-4-0125-preview
summary: "Manusear n\xFAmeros complexos em TypeScript requer uma classe dedicada."
title: "Trabalhando com n\xFAmeros complexos"
weight: 14
---

## Como fazer:
Manusear números complexos em TypeScript requer uma classe dedicada. Vamos criar uma e trabalhar com adição e multiplicação.

```TypeScript
class Complexo {
    constructor(public re: number, public im: number) {}

    add(outro: Complexo): Complexo {
        return new Complexo(this.re + outro.re, this.im + outro.im);
    }

    multiply(outro: Complexo): Complexo {
        return new Complexo(
            this.re * outro.re - this.im * outro.im,
            this.re * outro.im + this.im * outro.re
        );
    }

    toString(): string {
        return `${this.re} + ${this.im}i`;
    }
}

let num1 = new Complexo(1, 2);
let num2 = new Complexo(3, 4);
let soma = num1.add(num2);
let produto = num1.multiply(num2);

console.log(`Soma: ${soma.toString()}`); // Saída: Soma: 4 + 6i
console.log(`Produto: ${produto.toString()}`); // Saída: Produto: -5 + 10i
```

## Aprofundamento
Historicamente, os números complexos foram controversos - até chamados de 'imaginários' para expressar o ceticismo inicial. Agora, eles são fundamentais na matemática e na ciência modernas.

Alternativas à nossa classe simples podem envolver o uso de bibliotecas existentes como `math.js` ou `complex.js`, detalhadas com recursos adicionais como funções trigonométricas, exponenciação e conjugação complexa.

Nosso detalhamento da implementação em TypeScript se resume a definir operações aritméticas. O método `add` simplesmente adiciona as partes correspondentes. `multiply` aplica o método FOIL usado em álgebra, lembrando que `i^2 = -1`.

## Veja Também
Para mais leituras e recursos sobre números complexos e seu uso na programação, confira:

- Álgebra de Números Complexos da MDN: https://developer.mozilla.org/pt-BR/docs/Web/JavaScript/Reference/Global_Objects/BigInt
- Biblioteca `math.js`: https://mathjs.org/docs/datatypes/complex_numbers.html
- Biblioteca `complex.js`: https://complex-js.github.io/complex.js/
