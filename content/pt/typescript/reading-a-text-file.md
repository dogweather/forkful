---
title:                "Lendo um arquivo de texto."
html_title:           "TypeScript: Lendo um arquivo de texto."
simple_title:         "Lendo um arquivo de texto."
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/typescript/reading-a-text-file.md"
---

{{< edit_this_page >}}

## O que e por que?

Ler um arquivo de texto é o processo de ler e interpretar o conteúdo de um arquivo de texto. Programadores frequentemente fazem isso para acessar ou manipular informações armazenadas em um arquivo, como dados de configuração ou texto formatado. É uma maneira útil de interagir com arquivos de texto sem ter que abri-los manualmente e ler o conteúdo.

## Como fazer:

```TypeScript
// Importando a biblioteca fs do Node.js
import * as fs from 'fs';

// Lendo um arquivo de texto de forma síncrona
const fileContent = fs.readFileSync('arquivo.txt', 'utf-8');
console.log(fileContent);

// Lendo um arquivo de texto de forma assíncrona
fs.readFile('arquivo.txt', 'utf-8', (err, data) => {
  if (err) {
    // Caso ocorra um erro, ele será impresso no console
    console.log(err);
  } else {
    // Caso contrário, o conteúdo do arquivo será impresso no console
    console.log(data);
  }
});

```

A saída do código acima depende do conteúdo do arquivo "arquivo.txt". Se o arquivo possuir o texto "Hello world!", a saída será "Hello world!".

## Aprofundando:

Ler arquivos de texto é uma tarefa comum em programação e existem várias opções para fazê-lo. Além do método explicado acima, também é possível usar a biblioteca "fs-extra" para ler arquivos de forma mais simples e com mais recursos. Além disso, o TypeScript também possui a opção de ler arquivos usando "streams", o que pode ser mais eficiente para arquivos grandes.

## Veja também:

- Documentação oficial do Node.js sobre a biblioteca "fs": https://nodejs.org/api/fs.html
- Biblioteca "fs-extra" para ler arquivos no Node.js: https://github.com/jprichardson/node-fs-extra
- Tutorial sobre como ler arquivos de texto com streams no TypeScript: https://www.digitalocean.com/community/tutorials/reading-files-with-node-js