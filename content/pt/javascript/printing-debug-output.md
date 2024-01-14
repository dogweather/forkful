---
title:    "Javascript: Imprimir saída de depuração"
keywords: ["Javascript"]
---

{{< edit_this_page >}}

Por que imprimir saída de depuração em JavaScript?

A depuração é um processo importante no desenvolvimento de qualquer código, incluindo JavaScript. Imprimir saída de depuração permite aos programadores visualizarem informações adicionais durante a execução do código, o que pode ser útil para identificar erros e acompanhar o fluxo do programa.

Como imprimir saída de depuração em JavaScript?

Há várias maneiras de imprimir saída de depuração em JavaScript, sendo a mais comum o uso do console.log(). Este método permite imprimir valores de variáveis ​​e mensagens de texto no console do navegador ou ambiente de desenvolvimento. Por exemplo:

```Javascript
let nome = "João";
console.log("Olá, meu nome é " + nome);
```

Este código imprimirá a seguinte saída no console:

```
Olá, meu nome é João
```

Além do console.log(), também é possível utilizar o comando debugger para interromper a execução do código e visualizar o valor de variáveis ​​em tempo real.

Profundidade na impressão da saída de depuração

Enquanto o console.log() é uma maneira rápida e fácil de imprimir saída de depuração, existem outras técnicas que permitem uma depuração mais detalhada. Por exemplo, o console.table() permite imprimir os valores de objetos e arrays como uma tabela, facilitando a visualização e comparação de dados.

Outro recurso útil é o uso de marcadores, como console.time() e console.timeEnd(). Estes permitem medir o tempo de execução de um trecho de código específico.

Veja também:

- [Documentação do console no MDN](https://developer.mozilla.org/pt-BR/docs/Web/API/Console)
- [Artigo sobre depuração em JavaScript](https://pt.stackoverflow.com/questions/37290/como-fazer-depura%C3%A7%C3%A3o-de-javascript-no-google-chrome)
- [Tutorial sobre depuração de código no Visual Studio Code](https://code.visualstudio.com/docs/nodejs/nodejs-debugging)