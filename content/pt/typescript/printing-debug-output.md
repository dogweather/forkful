---
title:    "TypeScript: Imprimindo saída de depuração"
keywords: ["TypeScript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pt/typescript/printing-debug-output.md"
---

{{< edit_this_page >}}

## Por Que

Há momentos em que escrever e imprimir declarações de depuração (debug) pode ser uma ferramenta útil para entender melhor o que está acontecendo em seu código. Essas declarações podem fornecer informações valiosas sobre variáveis, valores e fluxo de execução do seu programa. Portanto, é importante saber como utilizar esse recurso ao programar em TypeScript.

## Como Fazer

Para imprimir uma declaração de depuração em TypeScript, podemos utilizar a função `console.log()`, que é uma funcionalidade do JavaScript e também suportada pelo TypeScript. Por exemplo:

```TypeScript
let num1: number = 5;
console.log(num1); // saída: 5
```

Também é possível imprimir várias variáveis ou valores em uma única declaração, separando-os por vírgula:

```TypeScript
let name: string = "João";
let age: number = 25;
console.log(name, age); // saída: João 25
```

Além disso, podemos formatar a saída utilizando as expressões `string` e `number` dentro da função `console.log()`, dessa forma:

```TypeScript
let num1: number = 5;
console.log(`O número é ${num1}.`); // saída: O número é 5.
```

## Mergulho Profundo

A função `console.log()` é apenas uma maneira básica de imprimir declarações de depuração. Existem outras formas mais específicas de fazer isso, como utilizar a interface `console` do TypeScript, que oferece mais opções e recursos, como `console.error()` para imprimir mensagens de erro e `console.table()` para imprimir dados em formato de tabela.

Outra opção interessante é utilizar o depurador (debugger) do TypeScript, presente em algumas IDEs como o Visual Studio Code, que permite pausar a execução do seu código em pontos específicos e analisar variáveis, valores e o fluxo de execução em tempo real.

## Veja Também

- [Documentação oficial do TypeScript sobre declarações de depuração](https://www.typescriptlang.org/docs/handbook/basic-types.html#string-literal-types)
- [Artigo sobre como usar o depurador do TypeScript no Visual Studio Code](https://code.visualstudio.com/docs/editor/debugging)
- [Guia completo de como usar a interface `console` do TypeScript](https://developer.mozilla.org/pt-BR/docs/Web/API/console)