---
title:                "Trabalhando com CSV"
aliases: - /pt/typescript/working-with-csv.md
date:                  2024-02-03T19:21:24.788856-07:00
model:                 gpt-4-0125-preview
simple_title:         "Trabalhando com CSV"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/typescript/working-with-csv.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## O Que & Por Quê?

Trabalhar com CSV (Valores Separados por Vírgula) envolve a leitura e escrita de arquivos CSV, um formato comum de troca de dados usado devido à sua simplicidade e ampla compatibilidade entre diversas plataformas e linguagens. Programadores lidam com arquivos CSV para importar ou exportar dados de aplicações, bancos de dados e serviços, possibilitando a fácil manipulação e compartilhamento de dados.

## Como fazer:

Em TypeScript, você pode trabalhar com arquivos CSV por meio de código nativo ou utilizando bibliotecas de terceiros como `csv-parser` para leitura e `csv-writer` para escrita de arquivos CSV.

### Lendo CSV com `csv-parser`

Primeiro, instale o `csv-parser` via npm:

```
npm install csv-parser
```

Em seguida, leia um arquivo CSV assim:

```typescript
import fs from 'fs';
import csv from 'csv-parser';

const results = [];

fs.createReadStream('data.csv')
  .pipe(csv())
  .on('data', (data) => results.push(data))
  .on('end', () => {
    console.log(results);
    // Saída: Array de objetos, cada um representando uma linha no CSV
  });
```

Assumindo que `data.csv` contém:

```
name,age
Alice,30
Bob,25
```

A saída será:

```
[ { name: 'Alice', age: '30' }, { name: 'Bob', age: '25' } ]
```

### Escrevendo CSV com `csv-writer`

Para escrever em um arquivo CSV, primeiro instale o `csv-writer`:

```
npm install csv-writer
```

Depois, use-o da seguinte forma:

```typescript
import { createObjectCsvWriter as createCsvWriter } from 'csv-writer';

const csvWriter = createCsvWriter({
  path: 'out.csv',
  header: [
    {id: 'name', title: 'NOME'},
    {id: 'age', title: 'IDADE'}
  ]
});

const data = [
  { name: 'Alice', age: 30 },
  { name: 'Bob', age: 25 }
];

csvWriter
  .writeRecords(data)
  .then(() => console.log('O arquivo CSV foi escrito com sucesso'));
```

Este código escreve o seguinte em `out.csv`:

```
NOME,IDADE
Alice,30
Bob,25
```

Estes exemplos mostram como integrar o processamento de CSV nos seus projetos TypeScript de maneira eficiente, seja lendo dados para análise ou persistindo dados de aplicação externamente.
