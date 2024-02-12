---
title:                "Trabalhando com CSV"
aliases:
- /pt/elm/working-with-csv.md
date:                  2024-02-03T19:19:18.230753-07:00
model:                 gpt-4-0125-preview
simple_title:         "Trabalhando com CSV"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/elm/working-with-csv.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## O Que e Por Quê?

Trabalhar com CSV (Valores Separados por Vírgula) envolve a análise e geração de arquivos que armazenam dados tabulares num formato simples de texto puro. Isso é comumente praticado por programadores para possibilitar a fácil troca de dados entre diferentes aplicações ou para processar grandes conjuntos de dados de maneira eficiente e segura em termos de tipo dentro do Elm.

## Como Fazer:

Elm não possui suporte embutido para análise ou geração de CSV; em vez disso, pacotes de terceiros como o `panosoft/elm-csv` são frequentemente utilizados. Os exemplos abaixo destacam o uso básico desta biblioteca para análise e geração de CSV.

### Analisando CSV

Primeiro, você precisa adicionar o pacote CSV ao seu projeto Elm:

```bash
elm install panosoft/elm-csv
```

Então, você pode analisar uma string CSV em uma lista de registros. Um exemplo simples:

```elm
import Csv

csvData : String
csvData =
    "nome,idade\nJohn Doe,30\nJane Smith,25"

parseResult : Result String (List (List String))
parseResult =
    Csv.parse csvData

-- Saída de amostra: Ok [["nome","idade"],["John Doe","30"],["Jane Smith","25"]]
```

### Gerando CSV

Para gerar uma string CSV a partir de dados Elm, use a função `Csv.encode`:

```elm
import Csv

registros : List (List String)
registros =
    [ ["nome", "idade"]
    , ["John Doe", "30"]
    , ["Jane Smith", "25"]
    ]

csvOutput : String
csvOutput =
    Csv.encode registros

-- Saída de amostra: "nome,idade\nJohn Doe,30\nJane Smith,25\n"
```

Essa abordagem simplista permite que você integre funcionalidades CSV dentro de suas aplicações Elm, aproveitando o ambiente seguro para manipulação e troca de dados.
