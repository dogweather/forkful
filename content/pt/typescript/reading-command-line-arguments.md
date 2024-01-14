---
title:    "TypeScript: Lendo argumentos da linha de comando"
keywords: ["TypeScript"]
---

{{< edit_this_page >}}

## Por que ler argumentos da linha de comando?

Ler argumentos da linha de comando é uma habilidade fundamental para qualquer programador TypeScript, pois permite que você crie programas mais interativos e dinâmicos. Com a capacidade de ler argumentos, seu código pode ser adaptado às necessidades específicas do usuário, tornando-o mais versátil e eficiente.

## Como fazer

Existem algumas maneiras de ler argumentos da linha de comando em TypeScript. Vamos dar uma olhada em um exemplo simples:

```TypeScript
// Importando a biblioteca "process"
import * as process from 'process';

// Obtendo os argumentos da linha de comando usando o método "argv"
const args = process.argv;

// Acesse os argumentos individuais pelo seu índice
const arg1 = args[0];
const arg2 = args[1];

// Dando saída nos argumentos na janela do console
console.log(arg1, arg2);
```

Supondo que tenhamos salvado o código acima em um arquivo "cli.ts" e o executamos usando o comando `ts-node cli.ts arg1 arg2`, a saída seria "arg1 arg2" no console. Como você pode ver, ler argumentos da linha de comando é tão simples quanto acessá-los por meio do objeto "process" e do método "argv".

## Mergulho profundo

Além de acessar argumentos individuais, é possível usar pacotes de terceiros, como "minimist" ou "commander", para fazer o parsing de argumentos mais complexos em TypeScript. Esses pacotes permitem que você defina argumentos e opções específicos e os interprete em seu código.

Além disso, você também pode usar o conceito de "flags" para tornar seu código ainda mais robusto. Essas flags podem ser usadas para ativar ou desativar determinados recursos em seu programa, fornecendo uma experiência de usuário mais personalizada.

## Veja também

- [Documentação oficial do processo Nó](https://nodejs.org/docs/latest/api/process.html)
- [Documentação minimist](https://github.com/substack/minimist)
- [Documentação comandante](https://github.com/tj/commander.js)