---
title:    "Javascript: Lendo argumentos de linha de comando"
keywords: ["Javascript"]
---

{{< edit_this_page >}}

## Por que ler argumentos da linha de comando?

Quando se trata de programação em Javascript, é essencial entender como ler argumentos da linha de comando. Isso pode ser útil para muitas tarefas, como criar scripts personalizados, automatizar tarefas e até mesmo criar programas completos que possam ser executados a partir da linha de comando. Além disso, é uma habilidade fundamental para qualquer desenvolvedor que deseja trabalhar em ambientes de servidor backend.

## Como ler argumentos da linha de comando em Javascript

A maioria das linguagens de programação oferece suporte para ler argumentos da linha de comando, e em Javascript não é diferente. Para isso, usamos o objeto `process` que vem integrado ao Node.js. O objeto `process` possui uma propriedade chamada `argv`, que é um array que contém todos os argumentos passados na linha de comando. Veja o exemplo abaixo:

```Javascript
const args = process.argv.slice(2);
console.log(args);
```

O código acima irá imprimir no console todos os argumentos passados, excluindo os dois primeiros (que geralmente são o caminho do Node e o arquivo executável). Por exemplo, se executarmos o comando `node script.js argumento1 argumento2`, o output será:

```
["argumento1", "argumento2"]
```

Podemos acessar cada argumento individualmente através do índice do array `args`. Por exemplo, se quisermos acessar somente o primeiro argumento, podemos fazer `args[0]`.

## Uma análise mais aprofundada

Além do objeto `process`, também podemos usar o módulo `yargs` para facilitar ainda mais a leitura de argumentos da linha de comando em Javascript. O `yargs` possui uma sintaxe mais amigável e também nos permite definir opções e flags para os argumentos.

Para utilizar o `yargs`, precisamos primeiro instalá-lo em nosso projeto usando o gerenciador de pacotes NPM. Em seguida, importamos o módulo em nosso arquivo Javascript e definimos as opções e flags que desejamos através da função `options()` do `yargs`. Veja um exemplo abaixo:

```Javascript
const argv = require('yargs').options({
  nome: {
    alias: "n",
    describe: "Nome da pessoa"
  },
  idade: {
    alias: "i",
    describe: "Idade da pessoa"
  },
  ocupacao: {
    alias: "o",
    describe: "Ocupação da pessoa"
  }
}).argv;

console.log(`Olá ${argv.nome}. Você tem ${argv.idade} anos e trabalha como ${argv.ocupacao}.`);
```

Se executarmos o comando `node script.js -n João -i 25 -o desenvolvedor`, o output será:

```
"Olá João. Você tem 25 anos e trabalha como desenvolvedor."
```

## Veja também

Para saber mais sobre o uso de argumentos da linha de comando em Javascript, confira os links abaixo:

- [Documentação oficial do objeto process](https://nodejs.org/api/process.html#process_process_argv)
- [Documentação oficial do módulo yargs](https://www.npmjs.com/package/yargs)