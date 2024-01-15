---
title:                "Trabalhando com csv"
html_title:           "Elm: Trabalhando com csv"
simple_title:         "Trabalhando com csv"
programming_language: "Elm"
category:             "Elm"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/elm/working-with-csv.md"
---

{{< edit_this_page >}}

## Por que

Você provavelmente já se deparou com arquivos CSV em algum momento, seja trabalhando com dados, importando informações em uma planilha ou até mesmo gerando relatórios. Mas você já considerou trabalhar com esses arquivos usando a linguagem Elm? Neste artigo, vamos explorar como essa linguagem funcional pode facilitar o trabalho com CSV.

## Como Fazer

Primeiramente, vamos precisar instalar a biblioteca `elm-csv` em nosso projeto. Podemos fazer isso usando o gerenciador de pacotes `elm`, com o comando:

```Elm
elm install joakin/elm-csv
```

Em seguida, vamos importar a biblioteca em nosso código com a seguinte declaração:

```Elm
import Csv
```

Para ler um arquivo CSV, precisamos primeiro carregá-lo para a memória usando a função `Csv.Decode.file` e especificando o nome do arquivo e qual delimitador está sendo usado (geralmente é a vírgula). Em seguida, podemos chamar a função `Csv.Decode.decode` para decodificar os dados e transformá-los em uma lista de registros.

```Elm
-- Lê um arquivo CSV e o decodifica em uma lista de registros
mapeamentoCsv : Decoders (List Registro)
mapeamentoCsv =
    file "mapeamento.csv" (decode Csv.decoder)

type alias Registro =
    { coluna1 : String
    , coluna2 : String
    }

-- Registro obtido após decodificar o arquivo "mapeamento.csv"
registro : Registro
registro =
    { coluna1 = "valor1"
    , coluna2 = "valor2"
    }
```

Também podemos trabalhar com CSV em tempo real, em vez de apenas ler arquivos. Podemos criar um canal de entrada de texto e usar a função `Csv.Decode.stream` para decodificar esses dados dinamicamente.

```Elm
-- Canal de entrada
canal : Signal Channel.Channel String
canal =
    Channel.incoming <|
        Signal.constant
            "valor1, valor2\nvalor3, valor4"

-- Decodificação dinâmica do canal de entrada
mapeamentoCsv : Decoders (List Registro)
mapeamentoCsv =
    Signal.foldp (\texto -> decode Csv.decoder) %Vazio
```

## Mergulho Profundo

A biblioteca `elm-csv` também oferece várias funções úteis para trabalhar com arquivos CSV, como converter listas de registros em CSV com a função `Csv.Encode.encode` e escrever esses dados em um arquivo usando a função `Csv.Encode.file`.

Além disso, podemos manipular e filtrar os dados decodificados de acordo com nossas necessidades, usando funções como `List.map` e `List.filter`.

```Elm
mapeamentoFiltrado : List Registro
mapeamentoFiltrado =
    mapeamentoCsv
        |> Result.toMaybe
        |> Maybe.map (List.filter (\registro -> Registro.coluna1 == "valor1"))
        |> Maybe.withDefault []
```

Com o poder da linguagem Elm e a facilidade da biblioteca `elm-csv`, trabalhar com arquivos CSV pode se tornar uma tarefa muito mais agradável e eficiente.

## Veja Também

- Documentação oficial da biblioteca `elm-csv`: https://package.elm-lang.org/packages/joakin/elm-csv/latest/
- Tutorial sobre como trabalhar com CSV em Elm: https://www.enriched.io/elm-and-csv-part-1/
- Exemplos de projetos que utilizam a biblioteca `elm-csv`: https://github.com/topics/elm-csv