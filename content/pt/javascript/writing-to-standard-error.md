---
title:                "Javascript: Escrevendo para o erro padrão"
simple_title:         "Escrevendo para o erro padrão"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/javascript/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Por que escrever para o erro padrão?

Você pode estar se perguntando: por que eu deveria me preocupar em escrever para o erro padrão ao programar em Javascript? A resposta é simples: escrever para o erro padrão é uma maneira eficaz de identificar e lidar com possíveis erros em seu código. Ao escrever mensagens de erro específicas para o erro padrão, você pode facilitar a resolução de problemas e o processo de depuração do seu código.

## Como fazer isso:

Escrever para o erro padrão em Javascript é muito simples. Tudo que você precisa é da função `console.error()` e de uma mensagem de erro específica. Por exemplo:

```Javascript
console.error("Ops, parece que ocorreu um erro!");
```

Isso irá imprimir a mensagem de erro no seu console, permitindo que você identifique e localize o problema em seu código. Outra maneira útil de usar a função `console.error()` é indicar quais variáveis estão causando o erro. Por exemplo:

```Javascript
let num1 = 5;
let num2 = "10";

if (typeof num2 !== "number") {
  console.error("O valor de 'num2' deve ser um número!");
}
```

Isso irá imprimir a mensagem de erro caso a variável `num2` não seja um número, o que pode ajudar a identificar o problema em um código mais complexo.

## Aprofundando-se:

Além de apenas imprimir mensagens de erro no seu console, também é possível fazer mais com a função `console.error()` em Javascript. Por exemplo, você pode usar a função `console.trace()` juntamente com `console.error()` para imprimir uma pilha de chamadas, mostrando a sequência de ações que levou ao erro. Além disso, você também pode personalizar mensagens de erro com formatação de cores usando códigos de escape ANSI, o que pode tornar mais fácil e rápido identificar erros no seu código.

## Veja também:

- [Função console.error() na documentação do MDN](https://developer.mozilla.org/pt-BR/docs/Web/API/Console/error)
- [Como criar e lançar erros personalizados em Javascript](https://www.freecodecamp.org/news/how-to-create-and-throw-errors-in-javascript/)
- [Guia completo sobre uso do console em Javascript](https://flaviocopes.com/javascript-console/)