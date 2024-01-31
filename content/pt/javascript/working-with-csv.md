---
title:                "Trabalhando com CSV"
date:                  2024-01-19
html_title:           "Bash: Trabalhando com CSV"
simple_title:         "Trabalhando com CSV"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/javascript/working-with-csv.md"
---

{{< edit_this_page >}}

## What & Why?
CSV, ou "Valores Separados por Vírgula", são arquivos simples usados para armazenar dados tabulares. Programadores lidam com CSV por sua simplicidade e ampla compatibilidade com sistemas de planilhas e bancos de dados.

## How to:
Para ler e escrever CSV em JavaScript, frequentemente usamos a biblioteca `PapaParse`. Aqui está um exemplo básico:

```Javascript
// Incluindo PapaParse
const Papa = require('papaparse');
const fs = require('fs');

// Lendo CSV do arquivo
const csvFile = fs.readFileSync('dados.csv', 'utf8');
Papa.parse(csvFile, {
  complete: function(results) {
    console.log('Dados do CSV:', results.data);
  }
});

// Escrevendo CSV para arquivo
const dados = [
  ['nome', 'idade', 'email'],
  ['Ana', '28', 'ana@example.com'],
  ['João', '34', 'joao@example.com']
];

const csv = Papa.unparse(dados);
fs.writeFileSync('saida.csv', csv);
```

Output ao ler:
```
Dados do CSV: [[ 'nome', 'idade', 'email'], ['Ana', '28', 'ana@example.com'], ['João', '34', 'joao@example.com']]
```

Output ao escrever: Um arquivo `saida.csv` será criado com o conteúdo correspondente.

## Deep Dive
CSV existe desde o início dos anos 70 e era usado em programas de mainframe. Alternativas modernas incluem JSON e XML, mas CSV continua popular pelo seu formato legível e leve. Na prática, devemos lidar com a complexidade de codificação (como UTF-8), delimitadores e qualificadores de texto ao implementar a leitura/escrita de CSV.

## See Also
- Documentação do PapaParse: https://www.papaparse.com/docs
- MDN Web Docs sobre trabalhar com texto: https://developer.mozilla.org/en-US/docs/Web/API/Text
- CSV na Wikipedia: https://pt.wikipedia.org/wiki/Comma-separated_values
