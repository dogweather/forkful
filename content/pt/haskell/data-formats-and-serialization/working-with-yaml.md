---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:25:30.511433-07:00
description: "Como Fazer: Haskell n\xE3o possui suporte interno para o processamento\
  \ de YAML, mas voc\xEA pode usar bibliotecas de terceiros como `yaml` e `aeson`\
  \ para\u2026"
lastmod: '2024-03-13T22:44:46.645656-06:00'
model: gpt-4-0125-preview
summary: "Haskell n\xE3o possui suporte interno para o processamento de YAML, mas\
  \ voc\xEA pode usar bibliotecas de terceiros como `yaml` e `aeson` para analisar\
  \ e gerar dados YAML."
title: Trabalhando com YAML
weight: 41
---

## Como Fazer:
Haskell não possui suporte interno para o processamento de YAML, mas você pode usar bibliotecas de terceiros como `yaml` e `aeson` para analisar e gerar dados YAML. Aqui está como você pode começar:

### Lendo YAML
Primeiro, adicione o pacote `yaml` às dependências do seu projeto. Em seguida, você pode usar o seguinte exemplo para analisar um documento YAML simples:

```haskell
{-# LANGUAGE OverloadedStrings #-}

import Data.YAML
import Data.ByteString (ByteString)
import Control.Monad.IO.Class (liftIO)

-- Dados YAML de exemplo
yamlData :: ByteString
yamlData = "
name: John Doe
age: 30
"

-- Defina uma estrutura de dados que corresponda ao documento YAML
data Person = Person
  { name :: String
  , age :: Int
  } deriving (Show)

instance FromYAML Person where
  parseYAML = withMap "Person" $ \m -> Person
    <$> m .: "name"
    <*> m .: "age"

main :: IO ()
main = do
  let parsed = decode1 yamlData :: Either (Pos,String) Person
  case parsed of
    Left err -> putStrLn $ "Erro ao analisar YAML: " ++ show err
    Right person -> print person
```
A saída de amostra para o código acima pode parecer:
```
Person {name = "John Doe", age = 30}
```

### Escrevendo YAML
Para gerar YAML a partir de estruturas de dados Haskell, você pode usar as funcionalidades de codificação do pacote `yaml` conforme mostrado abaixo:

```haskell
{-# LANGUAGE OverloadedStrings #-}

import Data.YAML
import Data.ByteString.Lazy.Char8 (unpack)

-- Usando a estrutura de dados Person do exemplo anterior

person :: Person
person = Person "Jane Doe" 25

main :: IO ()
main = do
  let yamlData = encode1 person
  putStrLn $ unpack yamlData
```
A saída deste programa será uma string formatada em YAML:
```
name: Jane Doe
age: 25
```

Estes exemplos devem servir como ponto de partida para trabalhar com YAML em Haskell. Dependendo das suas necessidades, você pode querer explorar mais recursos e opções avançadas fornecidas por estas bibliotecas.
