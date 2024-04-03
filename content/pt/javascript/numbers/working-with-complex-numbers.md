---
date: 2024-01-26 04:42:30.649773-07:00
description: "N\xFAmeros complexos s\xE3o n\xFAmeros com uma parte real e uma parte\
  \ imagin\xE1ria (como 3 + 4i). Eles aparecem em v\xE1rios problemas de programa\xE7\
  \xE3o, especialmente no\u2026"
lastmod: '2024-03-13T22:44:46.956615-06:00'
model: gpt-4-0125-preview
summary: "N\xFAmeros complexos s\xE3o n\xFAmeros com uma parte real e uma parte imagin\xE1\
  ria (como 3 + 4i)."
title: "Trabalhando com n\xFAmeros complexos"
weight: 14
---

## O Quê & Por Quê?
Números complexos são números com uma parte real e uma parte imaginária (como 3 + 4i). Eles aparecem em vários problemas de programação, especialmente no processamento de sinais, computação quântica e na resolução de equações polinomiais. Programadores os manipulam para efetivamente resolver esses tipos de tarefas.

## Como:
JavaScript não possui suporte nativo para números complexos, mas você pode arregaçar as mangas e lidar com isso usando objetos e matemática. Aqui vai uma dica rápida.

```javascript
class NumeroComplexo {
  constructor(real, imaginario) {
    this.real = real;
    this.imaginario = imaginario;
  }

  add(outro) {
    return new NumeroComplexo(this.real + outro.real, this.imaginario + outro.imaginario);
  }

  // ...adicione mais métodos (subtrair, multiplicar, dividir) conforme necessário

  toString() {
    return `${this.real} + ${this.imaginario}i`;
  }
}

const a = new NumeroComplexo(1, 2);
const b = new NumeroComplexo(3, 4);
const resultado = a.add(b);

console.log(`Resultado: ${resultado}`); // Resultado: 4 + 6i
```

## Aprofundando
Números complexos existem desde o século XVI, graças ao matemático italiano Gerolamo Cardano. Eles se tornaram cruciais em vários campos, como engenharia e física. Na programação moderna, são chave para simulações e algoritmos que necessitam de multidimensionalidade.

Agora, JavaScript não é equipado de forma nativa para números complexos. Mas, além da opção faça você mesmo, você poderia usar bibliotecas matemáticas como math.js ou numeric.js. Elas oferecem o poder para lidar com números complexos de forma mais intensa, adicionando vantagens como mais operações, cálculo de magnitude e descoberta de argumento.

Por baixo do capô, quando você opera com números complexos, é como gerenciar dois números separados unidos pela cintura. Adição e subtração são jogadas diretas - combine o real com real, o imaginário com imaginário. Multiplicação e divisão ficam mais picantes com danças de termos cruzados e precisam de mais cuidado.

## Veja Também
- Documentação do MDN sobre JavaScript: https://developer.mozilla.org/pt-BR/docs/Web/JavaScript/A_re-introduction_to_JavaScript
- Math.js, uma biblioteca matemática incluindo números complexos: https://mathjs.org/docs/datatypes/complex_numbers.html
- Numeric.js, outra biblioteca: http://numericjs.com/documentation.html
- Um mergulho mais profundo nos números complexos (focado em matemática): https://mathworld.wolfram.com/ComplexNumber.html
