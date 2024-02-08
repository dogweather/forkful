---
title:                "Trabalhando com números complexos"
aliases:
- pt/typescript/working-with-complex-numbers.md
date:                  2024-01-26T04:46:22.655713-07:00
model:                 gpt-4-0125-preview
simple_title:         "Trabalhando com números complexos"

tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/typescript/working-with-complex-numbers.md"
---

{{< edit_this_page >}}

## O Quê & Por Quê?
Números complexos, consistindo de uma parte real e uma parte imaginária (normalmente escritos como a + bi), possibilitam cálculos que são impraticáveis ou impossíveis apenas com os reais. Programadores os utilizam em campos como processamento de sinal, computação quântica e matemática aplicada, onde representações de números em duas dimensões são essenciais.

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
