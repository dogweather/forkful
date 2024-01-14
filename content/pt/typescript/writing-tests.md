---
title:                "TypeScript: Escrevendo testes"
programming_language: "TypeScript"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/typescript/writing-tests.md"
---

{{< edit_this_page >}}

## Por que é importante escrever testes em TypeScript?

Em desenvolvimento de software, a qualidade é fundamental. Garantir que o código funcione corretamente e com consistência é essencial para garantir um produto de qualidade aos usuários. Por isso, escrever testes é uma prática fundamental que ajuda a garantir a qualidade do código. Além disso, testes bem escritos também facilitam a manutenção do código, tornando-o mais escalável e menos propenso a erros.

## Como escrever testes em TypeScript?

### Configurando um ambiente de teste

Antes de começar a escrever os testes, é necessário configurar um ambiente de teste apropriado. Para isso, é necessário instalar o framework de testes Jest através do gerenciador de pacotes NPM. Com o Jest instalado, é preciso criar um arquivo de configuração de testes e, em seguida, adicionar alguns scripts ao arquivo "package.json" para que os testes possam ser executados.

```TypeScript
npm install --save-dev jest
```

```TypeScript
// Configuração de teste (jest.config.js)

module.exports = {
  preset: 'ts-jest',
  testEnvironment: 'node',
};
```

```TypeScript
// Scripts adicionados ao arquivo "package.json"

"scripts": {
  "test": "jest",
  "test:watch": "jest --watch"
}
```

### Escrevendo testes unitários

Os testes unitários são o primeiro nível de testes a serem escritos. Eles são responsáveis por verificar se as funções e métodos individuais do código estão funcionando corretamente. Para escrever um teste unitário em TypeScript, é necessário importar a função ou método que será testado e utilizar o método `expect` do Jest para verificar o resultado esperado.

```TypeScript
import { soma } from './funcoes';

test('soma 1 + 2 é igual a 3', () => {
  expect(soma(1, 2)).toEqual(3);
});
```

### Escrevendo testes de integração

Os testes de integração verificam se as diferentes partes do código estão funcionando corretamente em conjunto. Para escrever um teste de integração em TypeScript, é necessário importar a classe ou módulo que será testado e utilizar o método `expect` do Jest para verificar o resultado esperado.

```TypeScript
import { Calculadora } from './calculadora';

test('multiplicação de 2 x 3 é igual a 6', () => {
  const calc = new Calculadora();
  expect(calc.multiplicar(2, 3)).toEqual(6);
});
```

## Uma profundidade maior sobre a escrita de testes em TypeScript

Além dos testes unitários e de integração, existem outros tipos de testes que podem ser escritos em TypeScript, como os testes de interface e testes de cobertura. Os testes de interface verificam se a interface do código está funcionando corretamente, enquanto os testes de cobertura medem a porcentagem do código que é coberta pelos testes.

Ao escrever testes em TypeScript, é importante utilizar também ferramentas de mocks para simular comportamentos específicos e evitar interações com recursos externos, como APIs. Além disso, é importante seguir boas práticas de escrita de testes, como manter os testes independentes entre si e cobrir diferentes cenários de uso do código.

## Veja também

- [Documentação do Jest](https://jestjs.io/pt-BR/)
- [Guia de como escrever testes unitários efetivos em TypeScript](https://blog.softwaremill.com/effective-unit-testing-in-typescript-angular-814f6c6435b5)
- [Como escrever testes de integração em TypeScript](https://itnext.io/testing-your-typescript-app-with-jest-and-ts-jest-1ef1353e8ce8)