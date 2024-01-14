---
title:                "TypeScript: Imprimindo saída de depuração"
programming_language: "TypeScript"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/typescript/printing-debug-output.md"
---

{{< edit_this_page >}}

## Por que imprimir saída de depuração?

A impressão de saída de depuração é uma técnica útil para programadores de TypeScript que desejam entender melhor seu código e corrigir erros. Com a impressão de saída de depuração, é possível ver o valor de variáveis no momento em que o código está sendo executado, o que pode ajudar a identificar erros e aprimorar o processo de desenvolvimento.

## Como fazer

Para imprimir a saída de depuração em TypeScript, podemos utilizar o comando `console.log()`. Este comando aceita múltiplos argumentos e irá imprimir seus valores no console do navegador ou do ambiente de desenvolvimento escolhido.

```
// Exemplo de impressão de saída de depuração
console.log("Olá mundo!", 123, true);
```

Este código irá imprimir a seguinte saída no console:

```
Olá mundo! 123 true
```

Podemos usar a impressão de saída de depuração em qualquer parte do nosso código, seja dentro de funções, loops ou condicionais. Também podemos imprimir o valor de variáveis ou objetos para verificar seu conteúdo durante a execução do código.

```
// Exemplo de uso da impressão de saída de depuração com variáveis
let nome = "Marina";
let idade = 30;

console.log("Olá, meu nome é " + nome + " e eu tenho " + idade + " anos.");
```

Saída:

```
Olá, meu nome é Marina e eu tenho 30 anos.
```

## Deep Dive

Além do comando `console.log()`, existem outras ferramentas que podemos utilizar para a impressão de saída de depuração em TypeScript. Por exemplo, o comando `console.warn()` é útil para imprimir mensagens de advertência no console. O comando `console.error()` pode ser utilizado para indicar erros no código, facilitando a identificação e correção dos mesmos.

Outra técnica comum é utilizar o método `debugger`. Este método pausa a execução do código e permite que o programador analise a pilha de chamadas e o valor das variáveis neste momento. No entanto, é importante lembrar de remover estes comandos de depuração antes de lançar o código em produção.

## Veja também

- [Documentação oficial do TypeScript sobre a impressão de saída de depuração](https://www.typescriptlang.org/docs/handbook/debugging.html)
- [Artigo sobre técnicas de depuração em TypeScript](https://blog.logrocket.com/debugging-typescript-with-visual-studio-code/)
- [Vídeo tutorial sobre impressão de saída de depuração em TypeScript](https://www.youtube.com/watch?v=4r4UleqWzX4)