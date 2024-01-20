---
title:                "Escrevendo testes"
html_title:           "Arduino: Escrevendo testes"
simple_title:         "Escrevendo testes"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/javascript/writing-tests.md"
---

{{< edit_this_page >}}

## O Que É & Porquê?

Escrever testes consiste em criar códigos que checam se outros códigos funcionam como esperado. Programadores testam para garantir qualidade e evitar bugs futuros.

## Como Fazer:

```javascript
// Exemplo com Jest
const soma = (a, b) => a + b;

test('soma 1 + 2 igual a 3', () => {
  expect(soma(1, 2)).toBe(3);
});
```

Saída:
```
PASS  ./soma.test.js
✓ soma 1 + 2 igual a 3 (5ms)
```

## Aprofundando o Conhecimento

Testes automatizados começaram nos anos 90 com o boom do desenvolvimento de software. Hoje, entre as alternativas, temos Jest, Mocha, Jasmine e QUnit para JavaScript. Testes podem ser unitários (pequenas partes do código), de integração (combinação de partes) ou end-to-end (fluxo completo do app). A implementação varia: para UI, pode-se usar algo como Cypress ou Selenium.

## Veja Também

- Jest: https://jestjs.io/
- Mocha: https://mochajs.org/
- Jasmine: https://jasmine.github.io/
- Cypress: https://www.cypress.io/
- Artigo sobre os diferentes tipos de testes: [Understanding different test types - Unit vs Integration vs E2E](https://kentcdodds.com/blog/unit-vs-integration-vs-e2e-tests)