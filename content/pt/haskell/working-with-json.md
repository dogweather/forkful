---
title:                "Trabalhando com json"
html_title:           "Haskell: Trabalhando com json"
simple_title:         "Trabalhando com json"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/haskell/working-with-json.md"
---

{{< edit_this_page >}}

## Por que JSON é importante?

JSON (JavaScript Object Notation) é um formato de dados amplamente utilizado para transmitir e armazenar informações estruturadas. Em Haskell, o uso de JSON é importante porque permite que os desenvolvedores interajam com APIs, sistemas de banco de dados e diversas outras fontes de dados externas.

# Como trabalhar com JSON em Haskell
Para começar a trabalhar com JSON em Haskell, você precisará instalar a biblioteca `aeson`. Em seguida, importe o módulo `Data.Aeson` para ter acesso às funções e tipos necessários para lidar com JSON. Abaixo está um exemplo de código mostrando como analisar e codificar dados JSON:

```Haskell
import Data.Aeson

-- Analisando dados JSON
jsonString = "{ \"nome\": \"João\", \"idade\": 25 }"

person = decode jsonString :: Maybe Person

data Person = Person { nome :: String, idade :: Int } deriving (Show, Generic)

instance FromJSON Person

-- Codificando dados para JSON
car = Car { modelo = "Fusca", ano = 1967, cor = "Azul" }

carJson = encode car
```

A saída do código acima será:

```
Just (Person {nome = "João", idade = 25})
"{ \"modelo\": \"Fusca\", \"ano\": 1967, \"cor\": \"Azul\" }"
```

## Mergulho Profundo
A biblioteca `aeson` é baseada em tipos de dados algébricos e usa o conceito de "lentes" para permitir a atualização de valores. Além disso, é possível criar instâncias personalizadas de `FromJSON` e `ToJSON` para tipos de dados definidos pelo usuário. Também é possível utilizar a função `parseEither` para tratar possíveis erros durante a análise de dados JSON.

Outra biblioteca útil para trabalhar com JSON em Haskell é a `json-autotype`, que permite gerar automaticamente tipos de dados com suas respectivas instâncias `FromJSON` e `ToJSON` a partir de um exemplo de dados JSON.

## Veja também
- [Haskell.org](https://www.haskell.org/)
- [Documentação do `aeson`](https://hackage.haskell.org/package/aeson)
- [Documentação do `json-autotype`](https://hackage.haskell.org/package/json-autotype)