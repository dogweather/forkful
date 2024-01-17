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

## O que é isso & Por que fazer isso?

CSV (comma-separated values) é um formato de arquivo comumente usado para armazenar dados tabulares, onde cada linha representa uma entidade e cada coluna representa um atributo. Programadores geralmente lidam com arquivos CSV para ler, processar e manipular esses dados de forma eficiente.

## Como fazer:

Para ler um arquivo CSV em JavaScript, podemos usar a biblioteca nativa *fetch* para recuperar o arquivo e a biblioteca *csv-parser* para processar o conteúdo. O código abaixo mostra um exemplo de como ler e imprimir os dados de um arquivo CSV:

```JavaScript
fetch("exemplo.csv")
  .then(response => response.text())
  .then(data => {
    const csv = require('csv-parser');
    csv(data)
      .on('data', (row) => {
        console.log(row);
      })
      .on('end', () => {
        console.log("Fim da leitura.");
      });
  })
  .catch(err => {
    console.error(err);
  });
```

O resultado deste código será um conjunto de objetos contendo os valores das colunas do arquivo CSV, como o exemplo mostrado abaixo:

```
[
  { nome: "João", sobrenome: "Silva", idade: 30 },
  { nome: "Maria", sobrenome: "Souza", idade: 25 }
]
```

## Mergulho Profundo:

CSV foi inventado na década de 1970 como um formato simples e eficiente para armazenar dados tabulares. Desde então, tornou-se amplamente utilizado em diversas áreas, como finanças, análise de dados e processamento de dados relacionais.

Existem várias bibliotecas em JavaScript para lidar com arquivos CSV, como o já mencionado *csv-parser*, o *csvtojson* e o *papaparse*. Cada uma dessas bibliotecas tem suas próprias características e vantagens, portanto, é importante escolher a que melhor atende às suas necessidades.

Ao trabalhar com arquivos CSV, é importante estar ciente de alguns desafios, como tratamento de aspas, delimitadores diferentes de vírgulas e caracteres especiais. Também é recomendado usar bibliotecas ao invés de implementar sua própria solução, pois elas já têm várias melhorias e correções de bugs implementadas.

## Veja Também:

- [Documentação oficial do JavaScript sobre a API *fetch*](https://developer.mozilla.org/pt-BR/docs/Web/API/Fetch_API)
- [Lista de bibliotecas em JavaScript para lidar com arquivos CSV](https://www.javascripting.com/chart-libraries/)