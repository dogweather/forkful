---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:19:52.150084-07:00
description: "Trabalhar com CSVs (Valores Separados por V\xEDrgula) envolve analisar\
  \ e gerar arquivos que armazenam dados tabulares em um formato de texto simples.\u2026"
lastmod: '2024-03-11T00:14:20.360328-06:00'
model: gpt-4-0125-preview
summary: "Trabalhar com CSVs (Valores Separados por V\xEDrgula) envolve analisar e\
  \ gerar arquivos que armazenam dados tabulares em um formato de texto simples.\u2026"
title: Trabalhando com CSV
---

{{< edit_this_page >}}

## O que & Por quê?

Trabalhar com CSVs (Valores Separados por Vírgula) envolve analisar e gerar arquivos que armazenam dados tabulares em um formato de texto simples. Programadores frequentemente se envolvem nessa tarefa para importar ou exportar dados de forma eficiente de planilhas, bancos de dados ou para facilitar a troca de dados entre diferentes programas.

## Como fazer:

Em Haskell, o manuseio de arquivos CSV pode ser realizado usando a biblioteca `cassava`, uma das bibliotecas de terceiros mais populares para esse propósito. Abaixo estão exemplos mostrando como ler de e escrever para arquivos CSV usando `cassava`.

**1. Lendo um arquivo CSV:**

Primeiro, certifique-se de ter a `cassava` instalada adicionando-a ao arquivo cabal do seu projeto ou usando Stack.

Aqui está um exemplo simples para ler um arquivo CSV e imprimir cada registro. Supomos que o arquivo CSV tenha duas colunas: nome e idade.

```haskell
{-# LANGUAGE OverloadedStrings #-}
import Data.Csv
import qualified Data.ByteString.Lazy as BL
import qualified Data.Vector as V

main :: IO ()
main = do
    csvData <- BL.readFile "people.csv"
    case decode NoHeader csvData of
        Left err -> putStrLn err
        Right v -> V.forM_ v $ \(name, age) ->
            putStrLn $ name ++ " tem " ++ show (age :: Int) ++ " anos de idade."
```

Assumindo que `people.csv` contenha:
```
John,30
Jane,25
```
A saída será:
```
John tem 30 anos de idade.
Jane tem 25 anos de idade.
```

**2. Escrevendo um arquivo CSV:**

Para criar um arquivo CSV, você pode usar a função `encode` da `cassava`.

Aqui está como você poderia escrever uma lista de registros em um arquivo CSV:

```haskell
{-# LANGUAGE OverloadedStrings #-}
import Data.Csv
import qualified Data.ByteString.Lazy as BL

main :: IO ()
main = BL.writeFile "output.csv" $ encode [("John", 30), ("Jane", 25)]
```

Após executar este programa, `output.csv` conterá:

```
John,30
Jane,25
```

Esta introdução concisa ao trabalho com arquivos CSV em Haskell usando a biblioteca `cassava` demonstra como ler e escrever em arquivos CSV, tornando as tarefas de manipulação de dados mais acessíveis para aqueles novos na linguagem.
