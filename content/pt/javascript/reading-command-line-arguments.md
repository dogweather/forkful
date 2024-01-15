---
title:                "Lendo argumentos da linha de comando"
html_title:           "Javascript: Lendo argumentos da linha de comando"
simple_title:         "Lendo argumentos da linha de comando"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/javascript/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Por que

Ao escrever um código em Javascript, é importante considerar que ele pode ser executado não apenas em um navegador, mas também no ambiente de linha de comando. Por isso, entender como ler os argumentos passados pela linha de comando pode ser muito útil para tornar seu código mais versátil e útil para diferentes propósitos.

## Como Fazer

Para ler os argumentos de linha de comando em Javascript, podemos usar o objeto `process` incorporado no Node.js. Ele contém uma propriedade `argv` que armazena um array com todos os argumentos passados pela linha de comando.

Veja um exemplo abaixo:

```Javascript
// arquivo: arguments.js
console.log(process.argv);
```

Se executarmos este código no terminal utilizando o comando `node arguments.js arg1 arg2`, teremos a seguinte saída:

```Javascript
["node", "arguments.js", "arg1", "arg2"]
```

Neste caso, o primeiro elemento do array é o caminho para o executável do Node.js, seguido pelo nome do arquivo que estamos executando e, por fim, os argumentos `arg1` e `arg2`.

Podemos também ler os argumentos individualmente, utilizando o índice do array. Por exemplo:

```Javascript
// arquivo: arguments.js
console.log(process.argv[2]);
console.log(process.argv[3]);
```

Com isso, se executarmos novamente o código com `node arguments.js arg1 arg2`, teremos na saída:

```Javascript
"arg1"
"arg2"
```

## Mergulho Profundo

O objeto `process` também contém outras propriedades úteis relacionadas aos argumentos de linha de comando, como `argv0` (que armazena o mesmo valor do índice 0 do array `argv`), `execArgv` (que armazena os argumentos de execução do Node.js) e `execPath` (que armazena o caminho para o executável do Node.js). Além disso, é possível utilizar módulos externos, como o `yargs`, para facilitar o processamento e validação de argumentos.

Vale lembrar também que no ambiente de linha de comando, podemos utilizar o JavaScript ES6 com o auxílio do Babel, possibilitando o uso de funcionalidades como rest parameters e destructuring assignment.

## Veja Também

- [Documentação do objeto `process` no Node.js (em inglês)](https://nodejs.org/docs/latest/api/process.html)
- [Módulo `yargs` para processamento de argumentos de linha de comando (em inglês)](https://www.npmjs.com/package/yargs)
- [Página oficial do Babel (em inglês)](https://babeljs.io/)