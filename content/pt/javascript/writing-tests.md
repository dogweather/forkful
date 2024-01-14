---
title:    "Javascript: Escrevendo testes"
keywords: ["Javascript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pt/javascript/writing-tests.md"
---

{{< edit_this_page >}}

## Por que escrever testes é importante?

Escrever testes é uma prática essencial para garantir que o código que você escreve está funcionando corretamente. Além disso, testes automatizados ajudam a evitar erros e problemas no futuro, economizando tempo e recursos.

## Como escrever testes em Javascript

Para escrever testes em Javascript, é importante utilizar uma ferramenta de teste, como o framework "Jest". Veja abaixo um exemplo de como escrever um teste simples para uma função de soma:

```Javascript
// Função de soma
function soma(a, b) {
  return a + b;
}

// Teste para verificar se a soma de 2 e 3 é igual a 5
test('soma 2 + 3 é igual a 5', () => {
  expect(soma(2, 3)).toBe(5);
});
```

Ao executar esse teste, o output deve ser:

```
PASS  test.js
√ soma 2 + 3 é igual a 5 (xms)`

Test Suites: 1 passed, 1 total
Tests: 1 passed, 1 total
```

Além disso, é possível utilizar diversas funções do jest, como `expect` e `toBe` para testar diferentes cenários e garantir a corretude do código.

## Aprofundamento em escrever testes

Além de testes unitários, existem também testes de integração e end-to-end. Os testes de integração garantem que várias partes do código estão funcionando juntas corretamente, enquanto os testes end-to-end simulam a interação de um usuário com a aplicação. É importante também cobrir o máximo possível de código com testes, e manter esses testes sempre atualizados à medida que o código é modificado.

## Veja também

- [Documentação oficial do Jest](https://jestjs.io/)
- [Artigo sobre testes em Javascript](https://tableless.com.br/testes-automaticos-javascript/)
- [Tutorial em vídeo sobre testes em Javascript](https://www.youtube.com/watch?v=r9HdJ8P6GQI)