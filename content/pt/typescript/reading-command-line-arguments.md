---
title:                "Lendo argumentos de linha de comando"
html_title:           "Arduino: Lendo argumentos de linha de comando"
simple_title:         "Lendo argumentos de linha de comando"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/typescript/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## O Que é e Porquê?

Ler argumentos da linha de comando é o processo de passar informações para um programa quando você o executa. Os programadores fazem isso para controlar o comportamento do programa durante a execução, permitindo uma maior flexibilidade e complexidade.

## Como Faz:

Vamos ver um exemplo de como ler argumentos da linha de comando em TypeScript. Vamos criar um programa simples que soma dois números que são passados como argumentos.

```TypeScript
// argumentos.ts
let num1 = parseInt(process.argv[2]);
let num2 = parseInt(process.argv[3]);

let soma = num1 + num2;

console.log(`A soma é ${soma}`);
```

Para executar este programa, usamos os elementos `num1` e `num2` do array `process.argv`. A saída será a soma dos dois números.

```bash 
$ ts-node argumentos.ts 4 5
A soma é 9
```

## Mergulhando Fundo: 

Historicamente, a maneira de passar argumentos para programas em muitos sistemas operacionais é através da linha de comando. Embora existam outras alternativas, como por exemplo, fazer com que o programa leia de um arquivo ou de uma fonte de entrada padrão, a leitura de argumentos da linha de comando ainda é a maneira mais flexível e direta. Detalhes da implementação podem variar, mas em Node.js e, por extensão, TypeScript, usamos o objeto `Process`, e especificamente a propriedade `argv` deste objeto, para acessar os argumentos da linha de comando.

## Veja também: 

- [Processo Node.js](https://nodejs.org/api/process.html#process_process_argv)
- [Documentação TypeScript](https://www.typescriptlang.org/docs/)