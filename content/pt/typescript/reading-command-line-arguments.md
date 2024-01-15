---
title:                "Lendo argumentos de linha de comando"
html_title:           "TypeScript: Lendo argumentos de linha de comando"
simple_title:         "Lendo argumentos de linha de comando"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/typescript/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Por que

Ler argumentos da linha de comando é uma habilidade importante para qualquer desenvolvedor TypeScript. Isso permite que você crie aplicações mais dinâmicas e interativas, permitindo que os usuários forneçam entradas diretamente do terminal. Além disso, entender como funciona a leitura de argumentos da linha de comando pode ajudar na criação de ferramentas de linha de comando eficientes.

## Como Fazer

Ler argumentos da linha de comando em TypeScript é bastante simples. Você pode acessá-los através do objeto global `process` e do array `argv`. Veja um exemplo de código abaixo:

```TypeScript
// Importa o objeto 'process' do Node.js para o TypeScript
import * as process from 'process';

// Acessa os argumentos da linha de comando
let args: string[] = process.argv;

// Imprime o primeiro argumento (normalmente o caminho do arquivo do Node.js)
console.log(args[0]);

// Imprime o segundo argumento (normalmente o primeiro argumento fornecido pelo usuário)
console.log(args[1]);

// Imprime o número total de argumentos fornecidos
console.log(`Total de argumentos fornecidos: ${args.length - 2}`);
```

Saída do exemplo de código acima (assumindo que o nome do arquivo seja `args.ts` e o usuário forneça o argumento `Hello World` na linha de comando):

```
node args.ts
args.ts
Total de argumentos fornecidos: 1
```

Você pode usar esta mesma lógica para lidar com argumentos adicionais e até mesmo criar funcionalidades específicas para cada argumento. O importante é entender que a leitura de argumentos da linha de comando é muito útil e fácil de implementar com TypeScript.

## Profundidade

Além de acessar os argumentos da linha de comando em um array, você também pode usar a biblioteca `minimist` para facilitar a leitura dos mesmos. Por exemplo, você pode usar a opção `--name` para fornecer o nome do usuário enquanto executa o seu programa. O código de exemplo abaixo mostra como fazer isso.

```TypeScript
// Instala e importa a biblioteca 'minimist'
import * as minimist from 'minimist';

// Acessa os argumentos da linha de comando com a biblioteca 'minimist'
let args = minimist(process.argv.slice(2));

// Imprime o valor fornecido para '--name'
console.log(`Olá, ${args.name}! Bem-vindo ao meu programa.`);
```

Saída do exemplo de código acima (assumindo que o nome do arquivo seja `hello.ts` e o usuário forneça o argumento `--name John` na linha de comando):

```
node hello.ts --name John
Olá, John! Bem-vindo ao meu programa.
```

Isso também mostra como a leitura de argumentos da linha de comando pode tornar suas aplicações mais interativas e personalizáveis. Portanto, não subestime a importância dessa habilidade e explore outras bibliotecas que possam facilitar ainda mais o processo de leitura de argumentos.

## Veja também

- [Documentação do objeto process](https://nodejs.org/api/process.html)
- [Documentação da biblioteca minimist](https://www.npmjs.com/package/minimist)