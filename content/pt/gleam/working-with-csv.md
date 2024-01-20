---
title:                "Trabalhando com CSV"
html_title:           "Bash: Trabalhando com CSV"
simple_title:         "Trabalhando com CSV"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/gleam/working-with-csv.md"
---

{{< edit_this_page >}}

## O Que é & Por Quê?
Trabalhar com CSV significa lidar com "Comma-Separated Values", um formato de arquivo usado para armazenar dados tabulares. Programadores o utilizam por ser simples de ler e escrever, além de ser amplamente suportado por diversas linguagens e ferramentas.

## Como Fazer:
```Gleam
import gleam/csv.{parser, Decoder}
import gleam/io.{File, Result}

fn main() -> Result(Nil) {
  let data = "nome,idade\nJoão,30\nAna,25"
  let decoder = Decoder(
    parser: parser.simple(),
    fields: #("nome", "idade")
  )

  case csv.decode(decoder, data) {
    Ok(rows) ->
      rows
      |> List.map(fn(row) {
        case row {
          Ok(record) -> record
          Error(_) -> "Erro na leitura do CSV"
        }
      })
      |> Result(Ok)

    Error(_) ->
      Error(Nil)
  }
}
```
Saída da amostra:
```
["João", "30"]
["Ana", "25"]
```

## Mergulho Profundo
CSV é um formato que data dos primeiros dias da computação pessoal; foi desenvolvido para facilitar a transferência de tabelas entre programas. Alternativas como JSON e XML oferecem mais complexidade e capacidade de representar estruturas aninhadas. Em Gleam, tratar CSV é mais manual comparado com outras linguagens: a biblioteca `gleam/csv` ajuda, mas você precisa definir decoders e lidar com erros linha a linha.

## Veja Também
- Documentação oficial do Gleam: [https://gleam.run/](https://gleam.run/)