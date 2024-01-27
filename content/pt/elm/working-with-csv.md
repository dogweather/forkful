---
title:                "Trabalhando com CSV"
date:                  2024-01-19
html_title:           "Bash: Trabalhando com CSV"
simple_title:         "Trabalhando com CSV"
programming_language: "Elm"
category:             "Elm"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/elm/working-with-csv.md"
---

{{< edit_this_page >}}

## O Que é & Por Que?
Trabalhar com CSV (Valores Separados por Vírgula) é o ato de ler e escrever em um formato de arquivo amplamente usado para troca de dados tabulares. Programadores usam CSV por sua simplicidade e interoperabilidade com diversas ferramentas, incluindo planilhas e bancos de dados.

## Como Fazer:
Elm não tem uma biblioteca padrão para CSV, mas você pode usar a biblioteca `elm-csv` que facilita a leitura e escrita de CSVs. Aqui está um exemplo rápido de como você pode usá-la:

```Elm
import Csv

csvString : String
csvString =
    "nome,idade,profissão\nJoão,34,Engenheiro\nMaria,28,Designer"

parseCsv : Result String (List (List String))
parseCsv =
    Csv.decode csvString

-- O output será: Ok [["nome","idade","profissão"],["João","34","Engenheiro"],["Maria","28","Designer"]]
```

Para escrever em CSV, você pode fazer o seguinte:

```Elm
import Csv

data : List (List String)
data =
    [ ["nome", "idade", "profissão"]
    , ["João", "34", "Engenheiro"]
    , ["Maria", "28", "Designer"]
    ]

csvOutput : String
csvOutput =
    Csv.encode data

-- O output será: "nome,idade,profissão\nJoão,34,Engenheiro\nMaria,28,Designer"
```

## Mergulho Profundo
CSV existe desde os primeiros dias da computação pessoal e se tornou um padrão de facto para troca de dados tabulares. Embora JSON e XML ofereçam estruturas mais complexas, CSV permanece popular devido à sua simplicidade e legibilidade. Implementar um parser de CSV é relativamente direto, mas pode se complicar com dados que contêm vírgulas, novas linhas ou campos que contêm aspas. A `elm-csv` lida com essas complexidades por você.

## Veja Também
- Elm CSV package: [package.elm-lang.org/packages/lovasoa/elm-csv/latest/](https://package.elm-lang.org/packages/lovasoa/elm-csv/latest/)
- Introdução a Elm: [guide.elm-lang.org](https://guide.elm-lang.org/)
- CSV na Wikipedia (para ter uma visão completa do formato): [pt.wikipedia.org/wiki/Comma-separated_values](https://pt.wikipedia.org/wiki/Comma-separated_values)
