---
title:                "Javascript: Lendo argumentos da linha de comando"
programming_language: "Javascript"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/javascript/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Por que ler argumentos da linha de comando?

Ler argumentos da linha de comando é uma habilidade importante para qualquer programador. Isso permite que você crie programas interativos que possam receber entradas do usuário sem a necessidade de uma interface gráfica. Além disso, é uma forma mais eficiente de executar seu programa, pois você pode passar parâmetros diretamente na linha de comando ao invés de editá-los no código.

## Como fazer

Para ler argumentos da linha de comando em Javascript, usamos o objeto `process.argv`. Este objeto contém um array de argumentos passados na linha de comando, incluindo o caminho do executável usado para chamar o programa. Confira o código abaixo para um exemplo básico:

```Javascript
// programa de exemplo para ler argumentos da linha de comando

// imprime todos os argumentos passados na linha de comando
console.log(process.argv);

// imprime o segundo argumento, que é o primeiro após o caminho do arquivo
console.log(process.argv[2]);
```

Agora, se chamarmos este programa da seguinte forma: `node programa.js argumento1 argumento2`, a saída será a seguinte:

```
["caminho/do/executavel", "caminho/do/arquivo/programa.js", "argumento1", "argumento2"]
argumento1
```

Observe que o primeiro elemento do array é sempre o caminho do executável.

## Mergulho Profundo

Além de ler argumentos simples passados na linha de comando, também é possível passar parâmetros com chaves e valores. Para isso, você precisará usar alguma biblioteca de terceiros, como o `yargs` ou o `minimist`. Essas bibliotecas facilitam a identificação e obtenção dos parâmetros passados na linha de comando.

Outra coisa importante a saber é que os argumentos da linha de comando são sempre do tipo `string`, mesmo que você passe um número como argumento. Por isso, é necessário converter o tipo de dados conforme necessário.

## Veja Também

- [Documentação oficial do `process.argv`](https://nodejs.org/docs/latest/api/process.html#process_process_argv)
- [Biblioteca `yargs`](https://www.npmjs.com/package/yargs)
- [Biblioteca `minimist`](https://www.npmjs.com/package/minimist)