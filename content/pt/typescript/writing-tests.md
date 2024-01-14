---
title:    "TypeScript: Escrevendo testes"
keywords: ["TypeScript"]
---

{{< edit_this_page >}}

## Por que escrever testes em TypeScript?

Escrever testes é uma prática essencial para garantir a qualidade do código em qualquer linguagem de programação, e TypeScript não é exceção. Ao escrever testes, você pode identificar erros e bugs antes que eles se tornem um problema em produção, economizando tempo e esforço no longo prazo. Também é uma boa prática ter testes para garantir que seu código continue funcionando conforme você faz alterações ou adiciona novos recursos.

## Como escrever testes em TypeScript

Existem várias maneiras de escrever testes em TypeScript, mas a mais comum é usar uma biblioteca de testes como Jest ou Mocha. Essas bibliotecas fornecem uma estrutura para organizar e executar testes de forma eficiente.

Vamos dar um exemplo usando o Jest. Primeiro, você precisa criar um arquivo de teste com a extensão ".spec.ts". Nele, você pode escrever um teste simples para uma função de soma:

```TypeScript
// math.spec.ts

import { soma } from "./math";

describe("soma", () => {
  it("deve somar dois números corretamente", () => {
    expect(soma(2, 3)).toBe(5);
  });
});
```

Neste exemplo, importamos a função "soma" do arquivo "math.ts" e usamos a função "toBe" para verificar se o resultado é igual a 5. Agora, basta executar os testes com o comando "npm test" e você deve obter um resultado positivo.

## Deep Dive em testes em TypeScript

Além de testar funções simples, é importante também testar componentes e classes do TypeScript. Para isso, você pode usar ferramentas como Enzyme ou React Testing Library. Essas ferramentas permitem simular a renderização de componentes e verificar seu estado e comportamento.

Também é importante lembrar de testar diferentes cenários, incluindo entradas inválidas e fluxos de erro. Além disso, os testes devem ser executados automaticamente sempre que houver uma alteração no código para garantir que tudo continue funcionando corretamente.

## Veja também

- [Jest](https://jestjs.io/)
- [Mocha](https://mochajs.org/)
- [Enzyme](https://enzymejs.github.io/enzyme/)
- [React Testing Library](https://testing-library.com/docs/react-testing-library/intro/)