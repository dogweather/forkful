---
title:                "Trabalhando com csv"
html_title:           "Javascript: Trabalhando com csv"
simple_title:         "Trabalhando com csv"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/javascript/working-with-csv.md"
---

{{< edit_this_page >}}

## Por que trabalhar com CSV?

Se você trabalha com dados estruturados, provavelmente já encontrou ou precisou trabalhar com arquivos CSV (comma-separated values). CSV é um formato amplamente utilizado para armazenar e transferir dados tabulares, como planilhas ou tabelas de banco de dados. Portanto, é importante para qualquer desenvolvedor de JavaScript estar familiarizado com a manipulação de CSV.

## Como fazer:

Para começar a trabalhar com CSV em JavaScript, é necessário ter uma compreensão básica do formato CSV.
Veja abaixo um exemplo de como ler um arquivo CSV e exibir cada linha de dados em um console:

```Javascript
const fs = require('fs');
const csv = require('csv-parser');

fs.createReadStream('arquivo.csv')
  .pipe(csv())
  .on('data', (data) => console.log(data));
```

O código acima utiliza o módulo `csv-parser` para analisar o arquivo CSV e exibir cada linha de dados no console. É importante notar que o módulo `fs` também é necessário para ler o arquivo CSV. Para fins de praticidade, o código assume que o arquivo CSV está no mesmo diretório do arquivo JavaScript.

Para adicionar e manipular dados em um arquivo CSV, utilize o módulo `csv-writer`:
```Javascript
const createCsvWriter = require('csv-writer').createObjectCsvWriter;

const csvWriter = createCsvWriter({
  path: 'output.csv',
  header: [
    {id: 'name', title: 'Nome'},
    {id: 'email', title: 'E-mail'},
  ]
});

const data = [
  {
    name: 'João',
    email: 'joao@email.com'
  },
  {
    name: 'Maria',
    email: 'maria@email.com'
  }
];

csvWriter
  .writeRecords(data)
  .then(() => console.log('Dados adicionados ao arquivo CSV com sucesso!'));
```

Esse código adicionará os dados no formato especificado ao arquivo CSV chamado "output.csv". É importante notar que o módulo `csv-writer` utiliza o conceito de cabeçalho (header) para especificar os campos de dados a serem adicionados.

Para um guia mais detalhado sobre como trabalhar com CSV em JavaScript, consulte a documentação dos módulos `csv-parser` e `csv-writer`.

## Mergulho profundo:

Trabalhar com CSV em JavaScript pode ser ainda mais simples utilizando a sintaxe de desestruturação e o método de array `map()`. Veja abaixo um exemplo de como adicionar dados em um arquivo CSV utilizando essa abordagem:

```Javascript
const fs = require('fs');
const csv = require('csv-parser');

let users = []; // array que armazenará os dados a serem adicionados ao arquivo CSV

fs.createReadStream('arquivo.csv')
  .pipe(csv())
  .on('data', (data) => {
    // utiliza a desestruturação para pegar somente os dados necessários
    const { name, email } = data;
    // adiciona um objeto com os dados a serem adicionados ao array users
    users.push({ name, email });
  })
  .on('end', () => {
    // utiliza o método map() para formatar os dados no formato necessário para o módulo csv-writer
    users = users.map(user => ({
      name: user.name.toUpperCase(), // transforma o nome em letras maiúsculas
      email: user.email.toLowerCase() // transforma o e-mail em letras minúsculas
    }));

    // adiciona os dados formatados ao arquivo CSV
    fs.appendFile('arquivo.csv', users, (err) => {
      if (err) throw err;
      console.log('Dados adicionados ao arquivo CSV com sucesso!');
    });
  });
```

Nesse exemplo, utilizamos o módulo `csv-parser` para ler o arquivo CSV e formatamos os dados antes de adicioná-los ao arquivo com o método `map()`. Essa abordagem é especialmente útil para manipular grandes quantidades de dados em arquivos CSV.

## Veja também:

- [Documentação do módulo csv-parser](https://www.npmjs.com/package/csv-parser)
- [Documentação do módulo csv-writer](https://www.npmjs.com/package/csv-writer)
- [Sintaxe de desestruturação em JavaScript](https://developer.mozilla.org/pt-BR/docs/Web/JavaScript/Reference/Operators/Destructuring_assignment)
- [Método map() em JavaScript](https://developer.mozilla.org/pt-BR