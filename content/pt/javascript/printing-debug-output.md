---
title:                "Javascript: Imprimindo saída de depuração"
simple_title:         "Imprimindo saída de depuração"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/javascript/printing-debug-output.md"
---

{{< edit_this_page >}}

## Introdução: Por que imprimir mensagens de depuração em um programa Javascript?

Se você já teve alguma experiência com programação, provavelmente já se deparou com a necessidade de debugar seu código. Isso significa identificar e corrigir erros e falhas em um programa de computador. Para ajudar nesse processo, uma técnica muito útil é a impressão de mensagens de depuração durante a execução do programa. Neste artigo, vamos explorar por que e como imprimir mensagens de depuração em programas Javascript.

## Como fazer: Exemplos de código e saída

Em Javascript, existem várias maneiras de imprimir mensagens de depuração, mas a forma mais comum é utilizando o comando console.log(). Isso permite que você imprima informações no console do navegador ou do ambiente de desenvolvimento.

Veja um exemplo simples de como utilizar o console.log() em um programa Javascript:

```Javascript
let nome = "Maria";
let idade = 28;

console.log(`O nome é ${nome} e a idade é ${idade}.`);
```

A saída deste código será:

```
O nome é Maria e a idade é 28.
```

Isso pode ser muito útil para verificar se as variáveis estão recebendo os valores esperados, ou para entender os valores e resultados de operações matemáticas ou lógicas.

Outra maneira de imprimir mensagens de depuração é utilizando o comando alert(), que exibe uma janela pop-up com a mensagem desejada. Veja um exemplo:

```Javascript
let numero = Math.random();

alert(`O número gerado é ${numero}.`);
```

A saída será uma janela pop-up mostrando o valor gerado pelo método Math.random().

Você também pode utilizar o console.table() para imprimir informações em forma de tabela, ou o console.error() para exibir mensagens de erro.

## Aprofundando: Maiores informações sobre impressão de mensagens de depuração

A impressão de mensagens de depuração é uma técnica essencial para encontrar e corrigir erros em seus programas. Ao imprimir informações relevantes durante a execução do código, você pode entender melhor o que está acontecendo e identificar onde o problema está ocorrendo.

Além disso, a impressão de mensagens de depuração pode ajudar a verificar se um determinado trecho de código está sendo executado ou não, o que é especialmente útil para testar condições e loops.

No entanto, é importante lembrar que é uma boa prática remover as mensagens de depuração antes de publicar o código para produção. Isso garante que seu programa não imprima informações sensíveis ou desnecessárias para o usuário final.

## Veja também

- [Documentação do console do Javascript](https://developer.mozilla.org/pt-BR/docs/Web/API/Console)
- [Tutorial de debug em Javascript](https://medium.com/javascript-inside/introduction-to-debugging-javascript-7c1dcb8a7c79)
- [Dicas para melhorar suas habilidades de debug](https://dev.to/rebeccauranga/dealing-with-the-wild-wild-world-of-debugging-3b7p)