---
title:                "Escrevendo testes"
date:                  2024-01-19
html_title:           "Arduino: Escrevendo testes"
simple_title:         "Escrevendo testes"

category:             "TypeScript"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/typescript/writing-tests.md"
---

{{< edit_this_page >}}

## O Que é & Por Que?

Escrever testes é criar verificações automatizadas para seu código. Programadores os utilizam para garantir que suas funções façam exatamente o que são supostas a fazer e nunca menos, nunca mais.

## Como Fazer:

Vamos usar Jest, uma biblioteca de testes para JavaScript e TypeScript. Instale-a com `npm install --save-dev jest @types/jest ts-jest`, e configure o Jest para TypeScript adicionando uma configuração `jest.config.js`.

```TypeScript
// soma.ts
export function soma(a: number, b: number): number {
  return a + b;
}

// soma.test.ts
import { soma } from './soma';

test('soma 1 + 2 igual a 3', () => {
  expect(soma(1, 2)).toBe(3);
});
```

Execute os testes com `npm test` ou `npx jest`. Você deverá ver algo como:

```
PASS  ./soma.test.ts
✓ soma 1 + 2 igual a 3 (5ms)
```

## Aprofundamento

Testes automatizados começaram na década de 1950 com programas que autotestavam suas próprias funções. Alternativas ao Jest incluem Mocha, Jasmine, e Ava. A decisão entre eles depende do gosto pessoal e necessidades específicas do projeto, como melhor integração CI/CD ou preferência de sintaxe.

Quanto aos detalhes, escrever bons testes envolve entender de mocks e spies para isolar componentes, e o conceito de test coverage para saber quanta do seu código está sendo testada.

## Veja Também

- Jest: https://jestjs.io/pt-BR/
- TypeScript com Jest: https://kulshekhar.github.io/ts-jest/
- Documentação de Testing Library: https://testing-library.com/docs/
- Jasmine: https://jasmine.github.io/
- Mocha: https://mochajs.org/
