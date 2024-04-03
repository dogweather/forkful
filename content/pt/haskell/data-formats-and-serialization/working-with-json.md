---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:22:48.248286-07:00
description: "Trabalhar com JSON (JavaScript Object Notation) em Haskell envolve analisar\
  \ dados JSON em tipos Haskell e converter tipos Haskell de volta para JSON.\u2026"
lastmod: '2024-03-13T22:44:46.646706-06:00'
model: gpt-4-0125-preview
summary: Trabalhar com JSON (JavaScript Object Notation) em Haskell envolve analisar
  dados JSON em tipos Haskell e converter tipos Haskell de volta para JSON.
title: Trabalhando com JSON
weight: 38
---

## Como:
Haskell não possui suporte embutido para JSON como JavaScript, mas com a ajuda de bibliotecas de terceiros como **Aeson**, manipular JSON se torna simples. Aeson oferece funções de alto e baixo nível para codificação (convertendo valores Haskell para JSON) e decodificação (analisando JSON para valores Haskell).

### Instalando Aeson
Primeiro, adicione Aeson às dependências do seu projeto atualizando seu arquivo `.cabal` ou usando Stack ou Cabal diretamente:

```shell
cabal update && cabal install aeson
```
ou, se você estiver usando Stack:
```shell
stack install aeson
```

### Analisando JSON
Vamos começar com um exemplo básico de decodificação de dados JSON em um tipo Haskell. Suponha que temos o seguinte JSON representando uma pessoa:

```json
{
  "name": "John Doe",
  "age": 30
}
```

Primeiro, defina um tipo de dados Haskell correspondente e faça dele uma instância de `FromJSON`:

```haskell
{-# LANGUAGE DeriveGeneric #-}

import GHC.Generics (Generic)
import Data.Aeson (FromJSON, decode)
import qualified Data.ByteString.Lazy as B

data Person = Person
  { name :: String
  , age :: Int
  } deriving (Generic, Show)

instance FromJSON Person

-- Função para decodificar JSON de um arquivo
decodePerson :: FilePath -> IO (Maybe Person)
decodePerson filePath = do
  personJson <- B.readFile filePath
  return $ decode personJson
```
Uso:
Assumindo que `person.json` contém os dados JSON mostrados acima, execute:
```haskell
main :: IO ()
main = do
  maybePerson <- decodePerson "person.json"
  print maybePerson
```
Saída de Amostra:
```haskell
Just (Person {name = "John Doe", age = 30})
```

### Codificando Valores Haskell como JSON
Para converter um valor Haskell de volta para JSON, você precisa fazer seu tipo uma instância de `ToJSON` e depois usar `encode`.

```haskell
import Data.Aeson (ToJSON, encode)
import GHC.Generics (Generic)

-- Supondo o tipo Person de antes

instance ToJSON Person

encodePerson :: Person -> B.ByteString
encodePerson = encode

main :: IO ()
main = do
  let person = Person "Jane Doe" 32
  putStrLn $ show $ encodePerson person
```
Saída de Amostra:
```json
{"name":"Jane Doe","age":32}
```

Estes exemplos demonstram o básico de trabalhar com JSON em Haskell usando Aeson. Lembre-se, Aeson oferece muito mais, incluindo regras de análise personalizadas, trabalho com JSON aninhado complexo, e muito mais, adequado para várias necessidades e cenários.
