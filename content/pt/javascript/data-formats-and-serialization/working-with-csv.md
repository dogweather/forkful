---
title:                "Trabalhando com CSV"
date:                  2024-02-03T19:20:16.613422-07:00
model:                 gpt-4-0125-preview
simple_title:         "Trabalhando com CSV"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/javascript/working-with-csv.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## O Que & Por Que?
Trabalhar com CSV (Valores Separados por Vírgula) em JavaScript envolve a análise ou geração de arquivos CSV para ingerir dados tabulares de fontes externas ou exportar dados para uso em outros programas. Programadores fazem isso porque possibilita uma troca de dados fácil e leve entre aplicações, bancos de dados e sistemas onde formatos mais complexos como JSON podem ser excessivos.

## Como Fazer:
JavaScript não possui funcionalidades embutidas para análise (parsing) de CSV ou para transformá-los em strings (stringifying), como acontece com JSON. No entanto, você pode gerenciar facilmente dados CSV usando JavaScript puro para tarefas mais simples ou aproveitando poderosas bibliotecas como `PapaParse` para cenários mais complexos.

### Análise Básica com JavaScript Puro
Para analisar uma simples string CSV em um array de objetos:

```javascript
const csv = `name,age,city
João,23,Nova Iorque
Jane,28,Los Angeles`;

function parseCSV(csv) {
  const lines = csv.split("\n");
  const result = [];
  const headers = lines[0].split(",");

  for (let i = 1; i < lines.length; i++) {
    const obj = {};
    const currentline = lines[i].split(",");
    
    for (let j = 0; j < headers.length; j++) {
      obj[headers[j]] = currentline[j];
    }
    result.push(obj);
  }
  
  return result;
}

console.log(parseCSV(csv));
```
Saída:

```
[
  { name: 'João', age: '23', city: 'Nova Iorque' },
  { name: 'Jane', age: '28', city: 'Los Angeles' }
]
```

### Geração Básica para CSV com JavaScript Puro
Para converter um array de objetos em uma string CSV:

```javascript
const data = [
  { name: 'João', age: 23, city: 'Nova Iorque' },
  { name: 'Jane', age: 28, city: 'Los Angeles' }
];

function arrayToCSV(arr) {
  const csv = arr.map(row => 
    Object.values(row).join(',')
  ).join('\n');
  
  return csv;
}

console.log(arrayToCSV(data));
```

Saída:

```
João,23,Nova Iorque
Jane,28,Los Angeles
```

### Usando PapaParse para Tarefas CSV Complexas
Para cenários mais complexos, o `PapaParse` é uma biblioteca robusta adequada para analisar e transformar arquivos CSV em strings com opções para fluxos, workers e manipulação de arquivos grandes.

Analisando arquivo CSV ou string com PapaParse:

```javascript
// Após adicionar PapaParse ao seu projeto
const Papa = require('papaparse');
const csv = `name,age,city
João,23,Nova Iorque
Jane,28,Los Angeles`;

Papa.parse(csv, {
  complete: function(results) {
    console.log("Analisado:", results.data);
  }
});
```

Gera:

```
Analisado: [
  ["name", "age", "city"],
  ["João", "23", "Nova Iorque"],
  ["Jane", "28", "Los Angeles"]
]
```

Transformando um array em uma string CSV com PapaParse:

```javascript
const data = [
  { name: 'João', age: 23, city: 'Nova Iorque' },
  { name: 'Jane', age: 28, city: 'Los Angeles' }
];

console.log(Papa.unparse(data));
```

Gera:

```
name,age,city
João,23,Nova Iorque
Jane,28,Los Angeles
```

Estes exemplos ilustram o manuseio básico e avançado de CSV em JavaScript, possibilitando uma troca de dados fácil em aplicações web e além.
