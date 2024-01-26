---
title:                "Trabalhando com CSV"
html_title:           "Bash: Trabalhando com CSV"
simple_title:         "Trabalhando com CSV"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/haskell/working-with-csv.md"
---

{{< edit_this_page >}}

## O Quê & Por Quê?

Trabalhar com CSV significa manipular arquivos "Valores Separados por Vírgula", úteis para armazenamento e transferência de dados tabelados. Programadores utilizam CSV pela simplicidade e interoperabilidade com diversas ferramentas e linguagens de programação.

## Como Fazer:

```Haskell
import qualified Data.ByteString.Lazy as BL
import qualified Data.Vector as V
import Data.Csv

type Pessoa = (String, Int, String)

main :: IO ()
main = do
    conteudoCsv <- BL.readFile "pessoas.csv"
    case decode NoHeader conteudoCsv of
      Left err -> putStrLn err
      Right v -> V.forM_ v $ \(nome, idade, profissao) ->
        putStrLn $ nome ++ " tem " ++ show idade ++ " anos e é " ++ profissao

-- Exemplo de saída para um CSV com conteúdo:
-- João,30,Engenheiro
-- Maria,25,Médica
--
-- Saida:
-- João tem 30 anos e é Engenheiro
-- Maria tem 25 anos e é Médica
```

## Mergulho Profundo

O formato CSV foi criado na década de 70 e tornou-se um padrão informal universal para intercâmbio de dados. Alternativas como JSON e XML oferecem estruturas mais complexas, mas CSV é rei em simplicidade e legibilidade. Na implementação, cascateio e análise (parsing) são detalhes importantes; bibliotecas como Cassava em Haskell facilitam esse processo, abstraindo as partes complicadas na manipulação de arquivos CSV.

## Veja Também

- Cassava: hackage.haskell.org/package/cassava
- Especificação CSV: tools.ietf.org/html/rfc4180
- Tutorial Haskell: learnyouahaskell.com/chapters
