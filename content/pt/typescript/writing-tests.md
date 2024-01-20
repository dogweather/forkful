---
title:                "Redação de testes"
html_title:           "TypeScript: Redação de testes"
simple_title:         "Redação de testes"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/typescript/writing-tests.md"
---

{{< edit_this_page >}}

## O que & Porquê?

Escrever testes é o processo de criar pequenos códigos que verificam se o seu código principal está funcionando como esperado. Os programadores fazem isso para garantir que as suas aplicações estão corretas e que o código está apto para ser implantado em produção.

## Como fazer:

Um exemplo simples de como escrever um teste em TypeScript:

```TypeScript 
// Código de exemplo em TypeScript

function add(x: number, y: number): number {
  return x + y;
}

test("Soma 2 + 3 e espera como resultado 5", () => {
  const expected = 5;
  const result = add(2, 3);
  expect(result).toBe(expected);
});
```

Saída esperada:

```
Test Soma 2 + 3 e espera como resultado 5 bem sucedido.
```

## Profundidade:

O processo de escrever testes é um processo importante para garantir que o código que você escreve está correto e livre de erros. Ele também pode ajudar a identificar problemas de lógica e a aumentar a manutenibilidade do seu código. Existem algumas alternativas ao uso de testes, como a revisão manual de código ou a execução de testes manualmente, mas esses métodos podem ser demorados e propensos a erros humanos.

## Veja também:

Para mais informações sobre testes com TypeScript, consulte:

- [Artigo sobre testes unitários com Jasmine e TypeScript](https://medium.com/@bryanmanuele/type-safe-jasmine-unit-testing-with-typescript-ebd8da1df0f2)