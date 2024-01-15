---
title:                "Trabalhando com arquivos csv"
html_title:           "TypeScript: Trabalhando com arquivos csv"
simple_title:         "Trabalhando com arquivos csv"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/typescript/working-with-csv.md"
---

{{< edit_this_page >}}

## Por que trabalhar com arquivos CSV?

Trabalhar com arquivos CSV pode ser extremamente útil para aqueles que desejam manipular grandes conjuntos de dados de forma eficiente. CSV (Comma-Separated Values) é um formato de arquivo simples e amplamente utilizado para armazenar dados tabulares, o que o torna uma opção popular para importação e exportação de dados em várias aplicações.

## Como fazer

Para começar a trabalhar com arquivos CSV em TypeScript, é necessário primeiro instalar o pacote "csv-parser" utilizando o gerenciador de pacotes npm. Depois disso, podemos usar o código abaixo para ler um arquivo CSV e imprimir seu conteúdo no console:

```typescript
import * as fs from 'fs';
import * as csv from 'csv-parser';

fs.createReadStream('arquivo.csv')
  .pipe(csv())
  .on('data', (data) => console.log(data));
```

O CSV é convertido em objetos JavaScript, facilitando assim a manipulação dos dados. Por exemplo, podemos adicionar filtros utilizando a função "filter" do JavaScript, ou então mapear os dados para formatos diferentes.

## Deep Dive

Além de simplesmente ler e imprimir os dados de um arquivo CSV, podemos realizar diversas operações mais complexas com ele. Por exemplo, podemos utilizar o pacote "fast-csv" para criar, atualizar e até mesmo excluir linhas em um arquivo CSV. Também podemos usar o "csv-writer" para escrever novos dados em um arquivo CSV de forma eficiente, sem sobrecarregar a memória com grandes conjuntos de dados.

Outra funcionalidade interessante é o uso de cabeçalhos personalizados no arquivo CSV. Podemos especificar manualmente os nomes das colunas e seus respectivos dados, tornando os arquivos mais legíveis e fáceis de serem manipulados.

## Veja também

- Documentação oficial do pacote "csv-parser" - https://www.npmjs.com/package/csv-parser
- Documentação oficial do pacote "fast-csv" - https://www.npmjs.com/package/fast-csv
- Documentação oficial do pacote "csv-writer" - https://www.npmjs.com/package/csv-writer