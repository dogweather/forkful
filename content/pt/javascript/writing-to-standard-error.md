---
title:                "Javascript: Escrevendo para o erro padrão"
programming_language: "Javascript"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/javascript/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Por que escrever para o erro padrão

A escrita para o erro padrão é uma prática comumente utilizada por programadores Javascript para lidar com erros e problemas em seus códigos. Isso permite que eles identifiquem e solucionem erros de maneira eficiente durante o desenvolvimento, melhorando a qualidade do código e a experiência do usuário final.

## Como fazer

Para escrever para o erro padrão em Javascript, basta utilizar o método `console.error()` seguido da mensagem de erro ou problema que deseja exibir. Por exemplo:

```
Javascript console.error("Ops! Ocorreu um erro.");
```

Isso exibirá a mensagem "Ops! Ocorreu um erro." no console do navegador ou terminal, indicando que algo está errado no código e permitindo que o desenvolvedor identifique e resolva o problema.

## Aprofundando-se

É importante entender que a escrita para o erro padrão não é apenas uma forma de exibir mensagens de erro, mas também pode ser utilizada para depurar e analisar o código. Ao adicionar informações detalhadas nas mensagens de erro, os programadores podem entender melhor o fluxo do programa e identificar onde exatamente ocorreu o problema.

Além disso, é possível formatar as mensagens de erro com variáveis e outros elementos do código, tornando-as ainda mais úteis e personalizadas. Por exemplo:

```
let numero = 5;
console.error("Ocorreu um erro ao tentar utilizar o número " + numero + ".");
```

Isso exibirá a mensagem "Ocorreu um erro ao tentar utilizar o número 5." no console, permitindo que o desenvolvedor saiba exatamente qual número causou o erro.

## Veja também

- [Método console.error() na documentação do Javascript](https://developer.mozilla.org/pt-BR/docs/Web/API/console/error)
- [Exibindo e lidando com erros em Javascript](https://www.freecodecamp.org/news/error-handling-in-javascript/)
- [Debugging no navegador com console e console.error](https://javascript.info/debugging-chrome)