---
title:                "Trabalhando com JSON"
date:                  2024-01-19
html_title:           "Arduino: Trabalhando com JSON"
simple_title:         "Trabalhando com JSON"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/haskell/working-with-json.md"
---

{{< edit_this_page >}}

## O Que & Porquê?
Trabalhar com JSON significa manipular um formato leve de troca de dados, bastante usado na web e em APIs. Programadores fazem isso para integrar sistemas, trocar informações entre cliente-servidor e salvar dados de forma legível por humanos e máquinas.

## Como Fazer:
Para trabalhar com JSON em Haskell, vamos usar a biblioteca `aeson`. Primeiro, instale com:

```
cabal install aeson
```

Agora, um exemplo de como decodificar e codificar JSON:

```Haskell
{-# LANGUAGE DeriveGeneric #-}

import Data.Aeson
import GHC.Generics (Generic)

-- Definindo um tipo de dado e instâncias para ToJSON e FromJSON
data Pessoa = Pessoa
  { nome :: String
  , idade :: Int
  } deriving (Show, Generic)

instance ToJSON Pessoa
instance FromJSON Pessoa

-- Decodificar JSON para o tipo Pessoa
exemploDecodificar :: ByteString -> Maybe Pessoa
exemploDecodificar = decode

-- Codificar uma Pessoa para JSON
exemploCodificar :: Pessoa -> ByteString
exemploCodificar = encode

main :: IO ()
main = do
  let jsonBytes = "{\"nome\":\"João\",\"idade\":30}"
      pessoa = exemploDecodificar jsonBytes
      novoJson = exemploCodificar (Pessoa "Ana" 25)

  print pessoa     -- Saída esperada: Just (Pessoa "João" 30)
  print novoJson   -- Saída esperada: "{\"nome\":\"Ana\",\"idade\":25}"
```

## Aprofundando
A `aeson` é a biblioteca padrão para JSON em Haskell, inspirada pela lib `json` do JavaScript. Outras alternativas incluem `jsonb` e `yaml`, cada uma com seus usos específicos. A implementação da `aeson` é eficiente e utiliza técnicas avançadas de Haskell, como typeclasses e generics para automação de código.

## Veja Também
- Documentação da biblioteca `aeson`: http://hackage.haskell.org/package/aeson
- Guia oficial da linguagem JSON: https://www.json.org/json-pt.html
- Tutorial de Haskell: http://learnyouahaskell.com/
