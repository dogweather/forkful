---
title:                "Lendo argumentos de linha de comando"
html_title:           "Arduino: Lendo argumentos de linha de comando"
simple_title:         "Lendo argumentos de linha de comando"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/javascript/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

# Artigo sobre programação em Javascript: A Leitura de Argumentos de Linha de Comando

## O Que & Por Que?
Ler argumentos de linha de comando é obter os detalhes passados para o seu programa durante a execução. Programadores fazem isso para controlar o comportamento do programa e flexibilizar seu uso.

## Como Fazer:
Aqui está um exemplo de como você pode usar o objeto `process.argv` para ler os argumentos de linha de comando em Javascript.

```Javascript
// process.argv irá conter uma array com os argumentos
process.argv.forEach((valor, indice) => {
  console.log(`${indice}: ${valor}`);
});
```
Quando você executa um programa como `node programa.js primeiro-argumento segundo-argumento`, ele imprimirá:
```Javascript
0: node
1: /caminho/para/programa.js
2: primeiro-argumento
3: segundo-argumento
```
Perceba que `process.argv` inclui o caminho para o node e para o arquivo do script em execução.

## Aprofundando
Historicamente, a leitura de argumentos de linha de comando remonta aos primórdios da computação, quando a interatividade era mínima e os programas eram controlados principalmente por meio desses argumentos. No ambiente Node.js, `process.argv` é o meio mais direto e fácil de acessar esses argumentos. Entretanto, se você precisa de algo mais sofisticado, existem bibliotecas como `commander` ou `yargs` que provêem um parsing de argumentos mais avançado.

Quando falamos em detalhes de implementação, vale lembrar que `process.argv` é uma array. O primeiro elemento é sempre 'node', e o segundo é o caminho para o arquivo de script em execução. Os argumentos da linha de comando começam no índice 2.

## Veja Também
Para mais informações sobre a leitura de argumentos de linha de comando em JavaScript, você pode visitar os links a seguir:

1. Documentação do `process.argv` na documentação do Node.js: https://nodejs.org/docs/latest/api/process.html#process_process_argv
2. Biblioteca `commander` para manipulação avançada de argumentos: https://www.npmjs.com/package/commander
3. Biblioteca `yargs` para parsing de argumentos: https://www.npmjs.com/package/yargs