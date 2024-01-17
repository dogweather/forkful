---
title:                "Lendo um arquivo de texto."
html_title:           "Javascript: Lendo um arquivo de texto."
simple_title:         "Lendo um arquivo de texto."
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/javascript/reading-a-text-file.md"
---

{{< edit_this_page >}}

## O que e Porque?

Ler um arquivo de texto é um processo essencial para programadores em Javascript. Trata-se de extrair informações de um arquivo de texto simples, como um documento de texto Word ou um arquivo CSV.

Os programadores fazem isso para acessar e processar dados armazenados em formato de texto. É uma maneira de importar dados para um programa e usá-los para realizar operações e manipulações.

## Como fazer:

```Javascript
// Criando uma variável para armazenar o arquivo de texto
var arquivo = require('arquivo.txt');

// Lendo o arquivo de texto
arquivo.readFile('arquivo.txt', 'utf8', function (err, data) {
  if (err) throw err;
  console.log(data);
});
```

## Aprofundando-se:

Ler arquivos de texto tem sido uma tarefa comum desde os primeiros dias da programação. Antigamente, os arquivos de texto eram utilizados como uma forma prática de armazenar dados. Hoje em dia, ainda é uma maneira popular de armazenar informações simples, principalmente em formatos como CSV ou JSON.

Além da forma tradicional de ler arquivos de texto, existem algumas alternativas que podem ser mais convenientes dependendo da situação. Por exemplo, para ler arquivos CSV, é possível usar bibliotecas específicas do CSV que já possuem funções para facilitar o processo de leitura e manipulação dos dados.

No quesito de implementação, existem algumas diferenças entre ler arquivos de texto em Javascript comparado com outras linguagens como C ou Java. Em Javascript, geralmente é mais simples e rápido, pois não é preciso manipular ponteiros e alocação de memória.

## Veja também:

- [Documentação oficial do Node.js sobre a leitura de arquivos](https://nodejs.org/api/fs.html#fs_fs_readfile_path_options_callback)
- [Biblioteca Papaparse para leitura de arquivos CSV em Javascript](https://www.papaparse.com/)