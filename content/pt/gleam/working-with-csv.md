---
title:                "Trabalhando com arquivos csv"
html_title:           "Gleam: Trabalhando com arquivos csv"
simple_title:         "Trabalhando com arquivos csv"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/gleam/working-with-csv.md"
---

{{< edit_this_page >}}

## O que é e porquê?

CSV é a abreviação de "Comma Separated Values" (Valores Separados por Vírgula) e é um formato de arquivo de texto que armazena dados em formato tabular, onde cada linha representa uma entrada de dados e cada coluna representa um campo. Programadores costumam trabalhar com CSV porque é uma maneira simples e eficiente de armazenar e manipular dados estruturados.

## Como fazer:

Para trabalhar com CSV em Gleam, precisamos importar o módulo `csv` utilizando a diretiva `use`:

```
use csv
```

Em seguida, podemos usar a função `read_file` para ler um arquivo CSV:

```
let result = csv.read_file("./dados.csv")
```

Podemos então iterar sobre o resultado utilizando `Enum.each` e imprimir os dados na tela:

```
Enum.each(result, fn(row) ->
  io.println(row)
end)
```

O resultado da execução será uma lista de listas, onde cada lista interna representa uma linha do arquivo CSV.

## Profundando:

CSV é um formato de arquivo muito popular, amplamente utilizado para trocar dados entre diferentes programas. Ele foi criado no início dos anos 1970 e se tornou um padrão para dados tabulares sem formatação fixa.

Além de Gleam, existem outras opções para trabalhar com CSV, como o módulo `csv` da linguagem de programação Elixir, ou a biblioteca `papaparse` para JavaScript.

No implementation details or historical context Brasil está cada vez mais inserido no mercado global de tecnologia, e saber como trabalhar com CSV é uma habilidade importante para qualquer programador. Com Gleam, temos uma opção poderosa e de fácil uso para lidar com esse formato de arquivo.

## Veja também:

- Documentação do módulo `csv` em Gleam: https://gleam.run/modules/csv.html
- Site oficial do formato CSV: https://tools.ietf.org/html/rfc4180
- Biblioteca `papaparse` para JavaScript: https://www.papaparse.com/