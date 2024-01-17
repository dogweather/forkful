---
title:                "Trabalhando com csv"
html_title:           "TypeScript: Trabalhando com csv"
simple_title:         "Trabalhando com csv"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/typescript/working-with-csv.md"
---

{{< edit_this_page >}}

## O que e por que?

Trabalhar com CSV (valores separados por vírgulas) é uma forma comum de armazenar e manipular dados em aplicações de software. Programadores usam essa formatação por ser simples, fácil de entender e compatível com muitas ferramentas e linguagens de programação.

## Como fazer:

Veja abaixo um exemplo de como trabalhar com CSV em TypeScript, mostrando como ler e escrever dados em um arquivo CSV:

```TypeScript
// Importar o módulo 'fs' para manipular arquivos
import fs from 'fs';

// Lê o arquivo CSV e converte para uma string
const csvData = fs.readFileSync('exemplo.csv', 'utf-8');

// Converte a string em um array de linhas
const csvLines = csvData.split('\n');

// Cria um array de objetos com cada linha sendo uma propriedade
const csvObjects = csvLines.map(line => {
  const [id, name, age] = line.split(','); // Cada linha é dividida por vírgula
  return { id, name, age };
});

// Escreve um novo arquivo CSV com as informações modificadas
csvObjects[0].age = 25; // Modifica a idade do primeiro objeto
fs.writeFileSync('novo.csv', csvObjects);

// Saída:
// novo.csv:
// id,name,age
// 1,John,25
// 2,Emily,32
```

## Profundidade:

CSV foi criado em 1972 como uma forma de salvar dados no formato de texto. Apesar de parecer antiquado, ainda é muito usado por ser fácil de entender e compatível com várias ferramentas e linguagens. Alternativas incluem formatos de dados mais complexos, como XML e JSON, mas eles podem ser menos eficientes para certos tipos de dados. Em TypeScript, a biblioteca `csv-parser` também é uma ótima opção para trabalhar com CSV.

## Veja também:

- [Documentação sobre CSV na Wikipédia](https://pt.wikipedia.org/wiki/CSV)
- [Api da biblioteca csv-parser](https://csv.js.org/parse/api/)
- [Exemplos de código com TypeScript e CSV](https://github.com/StratoDem/CSV.ts)