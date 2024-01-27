---
title:                "Trabalhando com CSV"
date:                  2024-01-19
html_title:           "Bash: Trabalhando com CSV"
simple_title:         "Trabalhando com CSV"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/typescript/working-with-csv.md"
---

{{< edit_this_page >}}

## What & Why?
Trabalhar com CSV (Valores Separados por Vírgula) é manipular dados em texto puro organizados por linhas e colunas. Programadores fazem isso pela simplicidade e universalidade do formato, que facilita a importação e exportação de dados para sistemas e ferramentas diversas.

## How to:
Para lidar com CSV em TypeScript, vamos utilizar a biblioteca `csv-parser`, que pode ser instalada via npm.

1. Instale o csv-parser:
```bash
npm install csv-parser
```

2. Supondo que você tenha um arquivo `dados.csv`:
```csv
nome,idade,profissao
Ana,25,Engenheira
Bruno,30,Designer
```

3. Crie um script TypeScript para ler e processar os dados CSV:
```typescript
import * as fs from 'fs';
import * as csv from 'csv-parser';

const results: any[] = [];

fs.createReadStream('dados.csv')
  .pipe(csv())
  .on('data', (data) => results.push(data))
  .on('end', () => {
    console.log(results);
    // [ { nome: 'Ana', idade: '25', profissao: 'Engenheira' },
    //   { nome: 'Bruno', idade: '30', profissao: 'Designer' } ]
  });
```

## Deep Dive
CSV é um formato padrão da indústria desde os primórdios da informática pela sua simplicidade. Alternativas como JSON ou XML oferecem estruturas mais complexas e metadados, mas CSV ainda é preferido para situações de troca rápida de dados tabelados. Quando se trata de implementação, TypeScript fornece abstrações fortes e tipagem, ajudando a manipular os dados CSV com mais confiança e segurança.

## See Also
- Documentação `csv-parser`: https://www.npmjs.com/package/csv-parser
- Parsing CSV com Node.js: https://nodejs.org/api/readline.html
- Dicas de TypeScript: https://www.typescriptlang.org/docs/handbook/utility-types.html
