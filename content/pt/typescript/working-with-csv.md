---
title:                "TypeScript: Trabalhando com csv"
simple_title:         "Trabalhando com csv"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/typescript/working-with-csv.md"
---

{{< edit_this_page >}}

## Por que trabalhar com CSV em TypeScript?

CSV, ou "Comma-Separated Values" (Valores Separados por Vírgula), é um formato de arquivo amplamente utilizado para armazenar dados tabulares. É uma opção popular para armazenar grandes quantidades de dados, pois é fácil de ler e interpretar. Além disso, praticamente todos os programas de planilha e banco de dados suportam esse formato. Neste post, discutiremos por que trabalhar com CSV em TypeScript e como fazê-lo de forma eficiente.

## Como fazer

Para trabalhar com CSV em TypeScript, precisamos usar a biblioteca "fast-csv", que pode ser instalada através do gerenciador de pacotes NPM. Com o "fast-csv", podemos ler e escrever em arquivos CSV usando apenas algumas linhas de código.

Primeiro, vamos importar a biblioteca "fast-csv" no nosso arquivo TypeScript:

```TypeScript
import * as csv from 'fast-csv';
```

Em seguida, podemos usar o método "fromPath" para ler um arquivo CSV existente e convertê-lo em um array de objetos TypeScript.

```TypeScript
csv.fromPath("arquivo.csv")  
.on("data", (data) => console.log(data))  
.on("end", () => console.log("Leitura do arquivo CSV concluída."));
```

No exemplo acima, usamos o evento "data" para imprimir as linhas do arquivo CSV e o evento "end" para indicar que a leitura do arquivo foi concluída.

Para escrever em um arquivo CSV, podemos usar o método "writeToPath" e passar um array de objetos como parâmetro. Por exemplo:

```TypeScript
const dados = [
    { nome: 'Pedro', sobrenome: 'Silva', idade: 25 },
    { nome: 'Maria', sobrenome: 'Souza', idade: 30 }
];

csv.writeToPath("novo-arquivo.csv", dados)
.on("finish", () => console.log("Dados salvos em CSV com sucesso."));
```

O código acima criará um novo arquivo CSV com os dados fornecidos pelo array "dados".

## Mergulho profundo

Além das funções básicas de leitura e escrita, o "fast-csv" possui muitos recursos úteis para trabalhar com arquivos CSV em TypeScript. Por exemplo, ele suporta a leitura de arquivos grandes, bem como a inserção de dados diretamente em um banco de dados.

Também é possível personalizar a leitura e escrita de CSV especificando opções como delimitadores, aspas e campos vazios. Isso permite que a biblioteca atenda às suas necessidades específicas ao trabalhar com diferentes formatos de CSV.

## Veja também

- [Documentação do "fast-csv"](https://www.npmjs.com/package/fast-csv)
- [Tutorial de CSV em TypeScript](https://www.digitalocean.com/community/tutorials/how-to-work-with-csv-files-in-typescript)
- [Exemplo prático de leitura e escrita de CSV em TypeScript](https://gist.github.com/samuelcampos/e5d0345a79369c61e26047b7505a9a2a)