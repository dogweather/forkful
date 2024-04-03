---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:19:18.230753-07:00
description: "Como Fazer: Elm n\xE3o possui suporte embutido para an\xE1lise ou gera\xE7\
  \xE3o de CSV; em vez disso, pacotes de terceiros como o `panosoft/elm-csv` s\xE3\
  o\u2026"
lastmod: '2024-03-13T22:44:46.521919-06:00'
model: gpt-4-0125-preview
summary: "Elm n\xE3o possui suporte embutido para an\xE1lise ou gera\xE7\xE3o de CSV;\
  \ em vez disso, pacotes de terceiros como o `panosoft/elm-csv` s\xE3o frequentemente\
  \ utilizados."
title: Trabalhando com CSV
weight: 37
---

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
