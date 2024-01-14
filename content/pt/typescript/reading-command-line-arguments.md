---
title:                "TypeScript: Leitura de argumentos da linha de comando"
simple_title:         "Leitura de argumentos da linha de comando"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/typescript/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Por que ler argumentos da linha de comando em TypeScript

Ler argumentos da linha de comando é uma habilidade essencial para qualquer programador que deseja criar aplicativos interativos e flexíveis. Ao entender como ler e manipular esses dados, você pode criar programas que se adaptam às necessidades do usuário e fornecem uma experiência personalizada. Neste artigo, vamos explorar como ler argumentos da linha de comando em TypeScript e mergulhar mais fundo no assunto para que você possa dominar essa habilidade.

## Como ler argumentos da linha de comando em TypeScript

Para ler argumentos da linha de comando em TypeScript, você precisará usar a classe [Process](https://nodejs.org/api/process.html) do Node.js. Esta classe fornece uma variedade de métodos que permitem acessar e manipular os argumentos passados para o seu programa. Vamos dar uma olhada em um exemplo simples de como ler argumentos da linha de comando usando o método `process.argv`:

````TypeScript
// Importa a classe Process
import * as process from 'process';

// Atribui os argumentos da linha de comando a uma variável
const args = process.argv;

// Exibe os argumentos no console
console.log(args);
````

Se você executar este código com alguns argumentos da linha de comando, como `node index.ts arg1 arg2 arg3`, você verá a seguinte saída:

````TypeScript
[
  '/usr/local/bin/node',
  '/Users/user/Documents/index.ts',
  'arg1',
  'arg2',
  'arg3'
]
````

Você pode usar esta array para acessar e manipular cada um dos argumentos individualmente. Por exemplo, se você quisesse exibir apenas o terceiro argumento, poderia usar `console.log(args[2])` e veria a saída `arg3`. Agora você pode começar a brincar com diferentes argumentos e utilizar a classe Process para lê-los e processá-los como precisar.

## Aprofundando na leitura de argumentos da linha de comando

Além do método `process.argv`, há outras duas maneiras de ler argumentos da linha de comando em TypeScript:

- Usando a biblioteca [commander](https://www.npmjs.com/package/commander) para criar uma interface de linha de comando mais robusta e fácil de usar.
- Usando o pacote [yargs](https://www.npmjs.com/package/yargs) que permite criar opções mais avançadas para seus argumentos da linha de comando.

Ambas as opções oferecem uma maneira mais organizada e fácil de ler e processar dados da linha de comando. No entanto, é importante lembrar que esses métodos requerem a instalação de pacotes adicionais através do gerenciador de pacotes do Node.js, o npm. Recomendamos que você leia a documentação desses pacotes para entender melhor seu funcionamento e escolher o que melhor atende às suas necessidades.

## Veja também

- [Documentação oficial da classe Process](https://nodejs.org/api/process.html)
- [Pacote commander no npm](https://www.npmjs.com/package/commander)
- [Pacote yargs no npm](https://www.npmjs.com/package/yargs)