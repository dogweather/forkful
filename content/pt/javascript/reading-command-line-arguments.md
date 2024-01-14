---
title:    "Javascript: Lendo argumentos da linha de comando"
keywords: ["Javascript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pt/javascript/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

# Por que ler argumentos de linha de comando em Javascript?

Ler argumentos de linha de comando pode ser extremamente útil para proporcionar interatividade e flexibilidade aos seus programas em Javascript. Se você é um programador iniciante ou experiente, entender como lidar com argumentos de linha de comando é essencial para o desenvolvimento de aplicações robustas e eficientes.

## Como fazer:

Para ler argumentos de linha de comando em Javascript, podemos usar o objeto `process` do Node.js. Este objeto possui uma propriedade `argv` que nos fornece uma matriz com todos os argumentos passados junto com o comando. Vamos ver um exemplo simples:

```Javascript
// salvando o segundo argumento passado na variável "nome"
let nome = process.argv[2];

// imprimindo uma mensagem de boas-vindas com o nome fornecido
console.log(`Olá, ${nome}! Seja bem-vindo ao meu programa.`);
```

Se executarmos o comando `node meuPrograma.js João`, o resultado será `Olá, João! Seja bem-vindo ao meu programa.` Claro que esse é apenas um exemplo básico, mas podemos criar programas mais complexos utilizando esses argumentos.

Além disso, podemos utilizar a biblioteca `yargs` para facilitar ainda mais o processamento de argumentos de linha de comando em Javascript. Com ela, podemos definir as opções e argumentos que esperamos receber e ainda mostrar mensagens de ajuda para o usuário. Veja um exemplo:

```Javascript
// importando o módulo "yargs"
const argv = require('yargs').argv;

// definindo as opções e argumentos
const opcoes = {
  nome: {
    alias: 'n',
    demandOption: true,
    describe: 'Nome do usuário',
    type: 'string'
  },
  idade: {
    alias: 'i',
    demandOption: true,
    describe: 'Idade do usuário',
    type: 'number'
  }
};

// mostrando mensagens de ajuda
const mensagemDeAjuda = 'Uso: node meuPrograma.js --nome <nome> --idade <idade>';
const opcoesDeAjuda = 'Opções: ' + JSON.stringify(opcoes, undefined, 2);
const mensagemResultado = 'Resultado: Meu nome é <nome> e tenho <idade> anos.';
const mensagens = '\nMensagem de ajuda: ' + mensagemDeAjuda + '\nOpções: \n' + opcoesDeAjuda  + '\nExemplo de resultado: \n' + mensagemResultado;

// recebendo os valores fornecidos pelo usuário
const nome = argv.nome;
const idade = argv.idade;

// imprimindo mensagem de boas-vindas
console.log(`Olá, ${nome}! Seja bem-vindo ao meu programa.`);

// mostrando mensagem de resultado
console.log(mensagens);
```

## Deep Dive:

Ler argumentos de linha de comando envolve entender os conceitos de entrada e saída no contexto de um programa. Ao receber argumentos, estamos fornecendo dados de entrada ao nosso programa, que serão processados e, em seguida, podemos ter uma saída baseada nesses dados. É importante entender como o objeto `process` funciona e como podemos manipular esses argumentos para obter o resultado desejado.

Além disso, é importante lembrar de tratar possíveis erros ao processar argumentos de linha de comando. Podemos utilizar as funções `hasOwnProperty()` ou `has()` para verificar se a opção ou argumento foi fornecido pelo usuário e, se necessário, lançar um erro.

## Veja também: 

- [Documentação do objeto `process`](https://nodejs.org/api/process.html)
- [Documentação da biblioteca `yargs`](https://github.com/yargs/yargs)
- [Tutorial de como ler argumentos de linha de comando em Javascript](https://www.digitalocean.com/community/tutorials/como-ler-argumentos-de-linha-de-comando-em-node-js-pt)