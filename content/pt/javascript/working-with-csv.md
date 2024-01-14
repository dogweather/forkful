---
title:                "Javascript: Trabalhando com csv"
simple_title:         "Trabalhando com csv"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/javascript/working-with-csv.md"
---

{{< edit_this_page >}}

## Por que trabalhar com CSV?

CSV (Comma Separated Values) é um formato de arquivo amplamente utilizado para armazenar dados tabulares, como planilhas e bancos de dados. Se você trabalha com análise de dados ou desenvolvimento web, provavelmente já precisou lidar com arquivos CSV. É uma forma popular de compartilhar e migrar dados entre diferentes sistemas, e aprender a trabalhar com ele pode ser uma habilidade valiosa na sua carreira de programação.

## Como fazer isso em Javascript

Felizmente, trabalhar com CSV em Javascript é bastante simples. Existem pacotes e bibliotecas disponíveis que tornam o processo de leitura e escrita de arquivos CSV muito mais fácil.

### Lendo arquivos CSV

Para ler um arquivo CSV em Javascript, você pode usar a biblioteca PapaParse. Ela oferece uma função `parse()` que converte um arquivo CSV em um objeto Javascript. Veja um exemplo de código usando essa biblioteca:

```javascript
const csvFile = "coluna1,coluna2,coluna3\nvalor1,valor2,valor3\nvalor4,valor5,valor6\n";
const csvData = Papa.parse(csvFile).data;
console.log(csvData);

// Output:
// [ [ 'coluna1', 'coluna2', 'coluna3' ],
//   [ 'valor1', 'valor2', 'valor3' ],
//   [ 'valor4', 'valor5', 'valor6' ] ]
```

### Escrevendo arquivos CSV

Para escrever um arquivo CSV em Javascript, podemos usar o pacote csv-writer. Ele fornece uma função `writeRecords()` que nos permite criar um arquivo CSV a partir de um array de objetos. Veja um exemplo de código usando esse pacote:

```javascript
const createCsvWriter = require('csv-writer').createArrayCsvWriter;

const csvWriter = createCsvWriter({
  header: ['nome', 'idade', 'cidade'],
  path: 'output.csv'
});

const data = [
  ['João', 25, 'São Paulo'],
  ['Maria', 30, 'Rio de Janeiro'],
  ['Pedro', 20, 'Belo Horizonte'],
];

csvWriter.writeRecords(data)
    .then(() => console.log('Arquivo CSV criado com sucesso!'));
```

Esse código irá criar um arquivo CSV chamado `output.csv` com os dados fornecidos.

## Deep Dive

Além das bibliotecas mencionadas, é possível trabalhar com CSV em Javascript utilizando algumas funções nativas da linguagem, como `split()` e `join()`. Também é importante estar atento às especificações do formato CSV, como o uso de aspas e delimitadores, para garantir a correta leitura e escrita dos arquivos.

Outra dica é utilizar a função `fs.readFile()` para ler arquivos CSV assincronamente, o que pode ser mais eficiente para arquivos grandes.

## Veja também

- [PapaParse](https://www.papaparse.com/)
- [csv-writer](https://www.npmjs.com/package/csv-writer)
- [Documentação do Node.js para manipulação de arquivos](https://nodejs.org/dist/latest-v14.x/docs/api/fs.html#fs_fs_readfile_path_options_callback)