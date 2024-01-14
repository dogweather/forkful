---
title:    "TypeScript: Escrevendo testes"
keywords: ["TypeScript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pt/typescript/writing-tests.md"
---

{{< edit_this_page >}}

## Por que escrever testes é importante?

Escrever testes é uma prática crucial para qualquer desenvolvedor TypeScript. Além de garantir que seu código funcione corretamente, os testes também ajudam a detectar bugs e a manter a qualidade do seu código. Com testes bem escritos, você pode ter mais confiança ao implementar novas funcionalidades e realizar alterações no seu código.

## Como escrever testes em TypeScript

Para escrever testes em TypeScript, você pode utilizar diversas bibliotecas de testes como o Jest, o Mocha ou o Chai. Vamos dar uma olhada em um exemplo básico utilizando o Jest:

```
import { soma } from './calculator';

test('Deve retornar o resultado da soma corretamente', () => {
  expect(soma(2, 2)).toBe(4);
});
```

Nesse exemplo, nós importamos a função *soma* do nosso arquivo de calculadora e criamos um teste para verificar se o resultado dessa função é igual a 4 quando passamos os valores 2 e 2 como parâmetros. Utilizamos o *expect* para definir qual é o resultado esperado e o *toBe* para verificar se o resultado é realmente igual ao esperado.

## Aprofundando em testes

Além dos testes unitários, que verificam a funcionalidade de uma parte isolada do código, também é importante escrever testes de integração e testes end-to-end. Os testes de integração garantem que diferentes partes do seu código funcionam juntas corretamente, enquanto os testes end-to-end simulam a interação de um usuário com a sua aplicação.

Outro conceito importante é a cobertura de testes, que significa quais partes do seu código estão sendo testadas e em que proporção. É importante ter uma boa cobertura de testes para garantir que todas as funcionalidades do seu código estão sendo testadas e que você está detectando possíveis bugs em todas as partes do seu projeto.

## Veja também

- [Documentação do TypeScript](https://www.typescriptlang.org/docs/)
- [Jest documentation](https://jestjs.io/docs/en/getting-started)