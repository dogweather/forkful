---
title:    "TypeScript: Escrevendo um arquivo de texto"
keywords: ["TypeScript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pt/typescript/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Por que escrever um arquivo de texto?

Escrever um arquivo de texto é uma tarefa fundamental para programadores em TypeScript e é essencial para realizar tarefas como armazenar informações, criar uma lista de dados ou simplesmente salvar dados temporariamente para uso futuro.

## Como fazer

Escrever um arquivo de texto em TypeScript é uma tarefa relativamente simples. Primeiro, precisamos importar o módulo 'fs' do Node.js, responsável por manipular arquivos. Podemos fazer isso usando o comando:

```TypeScript
import * as fs from 'fs';
```

Em seguida, podemos usar a função 'writeFileSync' fornecida pelo módulo 'fs' para escrever nosso arquivo de texto. Esta função recebe dois argumentos: o nome do arquivo que vamos criar e o conteúdo que queremos escrever. Por exemplo:

```TypeScript
fs.writeFileSync("arquivo.txt", "Este é um exemplo de conteúdo para o arquivo de texto");
```

Por fim, podemos verificar se nosso arquivo de texto foi criado corretamente, lendo o seu conteúdo e imprimindo no console. Para isso, utilizamos a função 'readFileSync' e o comando 'console.log' da seguinte maneira:

```TypeScript
console.log(fs.readFileSync("arquivo.txt", "utf-8"));
```

O output esperado será: "Este é um exemplo de conteúdo para o arquivo de texto". É importante ressaltar que o segundo argumento da função 'readFileSync' deve ser o mesmo passado na função 'writeFileSync'.

## Deep Dive

Além do método 'writeFileSync', o módulo 'fs' do Node.js também fornece o método 'writeFile', que é assíncrono e permite que outras operações sejam executadas ao mesmo tempo. Além disso, podemos especificar opções adicionais, como a codificação do arquivo ou umcallback para lidar com erros ou sucesso da operação.

Também é possível escrever em arquivos de texto que já existem, utilizando o método 'appendFileSync' para adicionar conteúdo ao final do arquivo. Isso pode ser útil para criar um log de informações ou salvar dados incrementalmente.

## Veja também

- [Documentação do módulo fs do Node.js](https://nodejs.org/dist/latest-v14.x/docs/api/fs.html)
- [Tutorial para escrever arquivos de texto em TypeScript](https://www.digitalocean.com/community/tutorials/how-to-read-and-write-files-in-typescript)
- [Vídeo tutorial sobre escrita de arquivos de texto com Node.js e TypeScript](https://www.youtube.com/watch?v=BnirQFP8mYo)