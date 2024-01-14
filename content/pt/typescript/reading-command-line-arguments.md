---
title:                "TypeScript: Lendo argumentos da linha de comando"
programming_language: "TypeScript"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/typescript/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Porque

Ao desenvolver aplicações TypeScript, muitas vezes é necessário interagir com o usuário por meio da linha de comando. Ler argumentos de linha de comando permite passar informações importantes para o programa de maneira dinâmica e flexível. Neste blog post, vamos aprender como fazer isso em TypeScript.

## Como Fazer

Para ler argumentos de linha de comando em TypeScript, podemos usar o objeto global `process`. Este objeto possui uma propriedade chamada `argv` que contém um array com todos os argumentos passados na linha de comando.

```TypeScript
// Exemplo de leitura de argumento de linha de comando
const argumento = process.argv[2]; // o primeiro argumento após "node index.ts"

console.log(argumento); // imprime o argumento na tela
```

Suponha que estejamos executando um programa chamado `index.ts` e passando o argumento `hello` na linha de comando. O código acima irá imprimir `hello` no terminal.

Outra forma de ler argumentos de linha de comando em TypeScript é usando a biblioteca `minimist`. Esta biblioteca permite acessar os argumentos por meio de um objeto, o que pode ser mais conveniente dependendo do tipo de aplicação que estamos desenvolvendo.

Para usar o `minimist`, primeiro precisamos instalar a biblioteca através do comando `npm install minimist`. Em seguida, podemos importá-la em nosso arquivo TypeScript e utilizá-la da seguinte maneira:

```TypeScript
import minimist from "minimist";

// Exemplo de uso do minimist
const args = minimist(process.argv.slice(2)); // slice remove "node index.ts"

console.log(args.hello); // imprime o valor do argumento --hello passado na linha de comando
```

## Deep Dive

Ler argumentos de linha de comando pode ser uma tarefa simples, mas pode ficar mais complexa dependendo da quantidade e complexidade dos argumentos passados. É importante ter em mente que os argumentos são sempre lidos como strings, então é necessário fazer a conversão para o tipo desejado, caso necessário.

Além disso, também é importante tratar possíveis erros ao ler os argumentos, como por exemplo quando um argumento obrigatório não é informado ou quando um argumento é passado com um formato inválido.

## Veja Também

- [Documentação oficial do objeto `process`](https://nodejs.org/docs/latest-v12.x/api/process.html)
- [Documentação oficial da biblioteca `minimist`](https://www.npmjs.com/package/minimist)