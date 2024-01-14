---
title:    "TypeScript: Leitura de argumentos da linha de comando"
keywords: ["TypeScript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pt/typescript/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Por que ler argumentos de linha de comando em TypeScript

Ao iniciar no desenvolvimento com TypeScript, pode ser confuso entender como ler e manipular os argumentos fornecidos pela linha de comando. No entanto, aprender a fazer isso pode trazer diversas vantagens para o seu código e tornar sua aplicação mais flexível e dinâmica. Neste artigo, vamos explorar o porquê de ler argumentos de linha de comando em TypeScript e como fazer isso de forma eficiente.

## Como ler argumentos de linha de comando em TypeScript

O TypeScript oferece diversas maneiras de ler argumentos de linha de comando, mas a mais comum é através da biblioteca `yargs`. Primeiramente, é necessário instalar esta biblioteca através do gerenciador de pacotes `npm`:

```TypeScript
npm install yargs
```

Uma vez instalado, podemos utilizá-la em nosso código com uma simples linha de importação:

```TypeScript
import yargs from 'yargs';
```

A partir disso, podemos acessar os argumentos fornecidos pela linha de comando utilizando o objeto `yargs.argv`. Por exemplo, se quisermos imprimir o primeiro argumento fornecido, poderíamos fazer da seguinte forma:

```TypeScript
console.log(yargs.argv._[0]);
```

Podemos também definir opções personalizadas para nossos argumentos e utilizá-las em nosso código. Para isso, utilizamos o método `options` da biblioteca `yargs` e passamos um objeto contendo as opções desejadas. Por exemplo, se quisermos definir uma opção `--name` que aceita uma string como valor, poderíamos fazer o seguinte:

```TypeScript
yargs.options({
  name: {
    type: 'string',
    demand: true,
    alias: 'n',
    description: 'Nome da pessoa',
  },
});
```

E para acessar o valor fornecido para essa opção, utilizamos `yargs.argv.name`. É possível também acessar as opções através de seus aliases, que são definidos no campo `alias` do objeto de opções. Para mais informações sobre as opções disponíveis da biblioteca `yargs`, consulte a documentação oficial.

## Mergulho profundo

Ler argumentos de linha de comando em TypeScript pode ser muito útil em diversas situações, tais como executar diferentes tarefas ou usar diferentes configurações dependendo do ambiente em que a aplicação estiver sendo executada. Além disso, o uso de opções personalizadas pode facilitar a interação com a aplicação, tornando-a mais amigável ao usuário.

É importante também lembrar de tratar os argumentos de forma adequada, validando suas entradas e lidando com possíveis erros. A biblioteca `yargs` oferece métodos para isso, como o `requiresArg` e o `coerce` que permitem validar e transformar os valores fornecidos para cada opção.

## Veja também

- Documentação oficial do `yargs`: https://www.npmjs.com/package/yargs
- Exemplos de leitura de argumentos de linha de comando em TypeScript: https://github.com/yargs/yargs/blob/master/example/basics.js