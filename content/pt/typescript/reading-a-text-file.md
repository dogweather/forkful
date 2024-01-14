---
title:    "TypeScript: Lendo um arquivo de texto."
keywords: ["TypeScript"]
---

{{< edit_this_page >}}

## Por que ler um arquivo de texto?

Ler arquivos de texto é uma habilidade fundamental em programação, permitindo que você armazene e manipule grandes quantidades de dados em seus projetos. Além disso, muitas vezes é necessário ler informações de arquivos de texto externos, como logs ou dados de configuração. Neste artigo, vamos aprender como ler um arquivo de texto usando TypeScript.

## Como fazer?

Antes de começarmos, é importante destacar que o TypeScript é uma linguagem de programação orientada a objetos, com forte tipagem e que transpila para JavaScript. Dito isso, vamos ao código!

Primeiro, vamos importar o módulo `fs` do Node.js para acessar as funções de leitura de arquivos. Em seguida, usamos o método `readFile()` para ler um arquivo de texto e armazená-lo em uma variável:

```TypeScript
import * as fs from 'fs';

let texto: string = fs.readFileSync('caminho/do/arquivo.txt');
```

Note que especificamos o caminho do arquivo a ser lido como parâmetro da função `readFileSync()`. Agora, podemos imprimir o conteúdo do arquivo na tela:

```TypeScript
console.log(texto);
```

Executando esse código, veremos o conteúdo do arquivo de texto sendo exibido no console.

## Mergulho Profundo

Além do método `readFile()`, o módulo `fs` oferece outras opções para leitura de arquivos de texto. Por exemplo, o método `readFileSync()` é síncrono, o que significa que o código será executado de forma sequencial, esperando a leitura do arquivo ser concluída antes de continuar. Porém, se você prefere uma abordagem assíncrona, pode usar o método `readFile()`:

```TypeScript
import * as fs from 'fs';

fs.readFile('caminho/do/arquivo.txt', (erro, data) => {
  if (erro) throw erro;
  console.log(data);
});
```

Nesse caso, passamos uma função callback como segundo parâmetro da função `readFile()`. Essa função será executada quando o arquivo for lido, passando como parâmetros possíveis erros e os dados do arquivo. Isso permite que o código continue sendo executado enquanto o arquivo é lido em background.

## Veja também

Aqui estão alguns links úteis para expandir seus conhecimentos sobre leitura de arquivos de texto em TypeScript:

- [Documentação do módulo fs do Node.js](https://nodejs.org/api/fs.html)
- [Tutorial de leitura de arquivos em TypeScript](https://codeburst.io/reading-a-file-with-typescript-82a41c4afe50)
- [Como ler um arquivo json em TypeScript](https://stackabuse.com/reading-and-writing-json-files-with-node-js/)