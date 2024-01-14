---
title:                "Javascript: Escrevendo testes"
programming_language: "Javascript"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/javascript/writing-tests.md"
---

{{< edit_this_page >}}

## Por que escrever testes é importante para programadores em Javascript?

Escrever testes é uma prática essencial para garantir a qualidade do código em qualquer linguagem de programação, incluindo Javascript. Ao escrever testes, podemos verificar se o nosso código está funcionando conforme o esperado e detectar possíveis problemas antes mesmo deles ocorrerem em produção. Isso nos ajuda a economizar tempo e recursos, além de garantir um produto final de melhor qualidade.

## Como escrever testes em Javascript

Existem várias ferramentas e frameworks disponíveis para escrever testes em Javascript, mas neste artigo vamos nos concentrar em dois deles: Mocha e Jest.

### Mocha

Mocha é um framework de teste popular que oferece uma sintaxe clara e simples para escrever testes. Para começar, instale o Mocha globalmente em seu projeto usando o npm:

```Javascript
npm install -g mocha
```

Em seguida, crie um arquivo de teste com o nome "test.js" e adicione o seguinte código:

```Javascript
// Importe a biblioteca de assertividade "chai"
var assert = require('chai').assert;

// Descreva o que o teste deve verificar
describe('Calculadora', function() {

  // Descreva o que o teste específico deve verificar
  it('deve retornar a soma de dois números', function() {
    // Execute o código que deve ser testado
    var resultado = soma(2, 3);

    // Verifique se o resultado é igual ao esperado
    assert.equal(resultado, 5);
  });

  // Também podemos testar se a função lança um erro
  it('deve lançar um erro se os parâmetros não forem números', function() {
    // Execute o código que deve ser testado
    function testarSoma() {
      soma('dois', 'três');
    }

    // Verifique se a função lança um erro
    assert.throws(testarSoma, Error);
  });
});

// Função que deve ser testada
function soma(a, b) {
  return a + b;
}
```

Para executar os testes, basta rodar o seguinte comando no terminal:

```
mocha test.js
```

### Jest

Jest é uma biblioteca de teste criada pelo Facebook e possui algumas vantagens em relação ao Mocha, como uma sintaxe mais simples e a capacidade de executar testes em paralelo. Para começar, instale o Jest em seu projeto com o npm:

```Javascript
npm install --save-dev jest
```

Em seguida, crie um arquivo de teste com o nome "app.test.js" e adicione o seguinte código:

```Javascript
// Descreva o que o teste deve verificar
describe('Teste de somar', () => {
  // Descreva o que o teste específico deve verificar
  test('deve retornar a soma de dois números', () => {
    // Execute o código que deve ser testado
    const resultado = soma(5, 5);

    // Verifique se o resultado é igual ao esperado
    expect(resultado).toBe(10);
  });
});

// Função que deve ser testada
function soma(a, b) {
  return a + b;
}
```

Para executar os testes, basta rodar o seguinte comando no terminal:

```
jest app.test.js
```

## Aprofundando-se em testes

Além dos exemplos apresentados, existem diversos outros conceitos e técnicas importantes para escrever testes eficazes em Javascript, como teste de integração, teste de unidade, mocks e spies. É importante estudar e praticar essas técnicas para se tornar um programador mais habilidoso e garantir a qualidade do seu código.

## Veja também

- [Documentação do Mocha](https://mochajs.org/)
- [Documentação do Jest](https://jestjs.io/pt-BR/)
- [Artigo sobre testes em Javascript](https://medium.com/xp-inc/testes-unit%C3%A1rios-no-javascript-com-jest-a-primeira-vista-dcf5a5e81da2)