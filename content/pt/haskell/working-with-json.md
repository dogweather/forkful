---
title:                "Haskell: Trabalhando com json"
simple_title:         "Trabalhando com json"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/haskell/working-with-json.md"
---

{{< edit_this_page >}}

##Por que trabalhar com JSON em Haskell?

JSON (JavaScript Object Notation) é um formato de dados popular que é amplamente usado para troca de dados em aplicativos da web. Por causa de sua legibilidade e facilidade de uso, o JSON é amplamente adotado por desenvolvedores e equipes de desenvolvimento. Ao trabalhar com Haskell, aprender a manipular e trabalhar com JSON pode ser muito útil no desenvolvimento de aplicativos da web e no processamento de dados.

##Como trabalhar com JSON em Haskell:

Para trabalhar com JSON em Haskell, precisamos primeiro importar o módulo `Data.Aeson`. Esse módulo fornece funções e tipos para trabalhar com JSON. Vamos dar uma olhada em um exemplo simples de como analisar um JSON e obter os dados de um campo específico usando a função `decode`:

```Haskell
import Data.Aeson

-- Definindo o JSON de entrada como uma string
json :: String
json = "{\"nome\": \"João\", \"idade\": 30}"

-- Criando um tipo de dados para representar os dados do JSON
data Pessoa = Pessoa { nome :: String, idade :: Int } deriving (Show, Generic)

-- Usando a função decode para analisar o JSON e obter os dados
pessoa :: Maybe Pessoa
pessoa = decode json :: Maybe Pessoa
```

Podemos acessar os campos específicos do JSON usando a notação de ponto, por exemplo, `nome pessoa` retornaria "João" e `idade pessoa` retornaria 30. Agora vamos ver como podemos transformar um tipo de dado em JSON usando a função `encode`:

```Haskell
-- Convertendo um valor do tipo Pessoa em JSON
pessoaToJson :: Pessoa -> Maybe String
pessoaToJson p = encode p
```

Com esses exemplos simples, podemos ver como é fácil trabalhar com JSON em Haskell.

##Mergulho Profundo:

Além da função `decode` e `encode`, o módulo `Data.Aeson` também fornece outras funções úteis para trabalhar com JSON, como `parseJSON` para analisar um valor JSON genérico e `toJSON` para converter um valor Haskell em JSON. Também é possível definir tipos de dados Haskell personalizados para corresponder diretamente aos campos de um JSON usando as anotações `FromJSON` e `ToJSON`. Além disso, o módulo `Data.Aeson` permite trabalhar com estruturas de dados aninhadas e lidar com dados opcionais usando a função `Maybe`.

##Ver também:

- A documentação do módulo `Data.Aeson` para obter uma lista completa de funções e tipos relacionados ao trabalhar com JSON em Haskell: https://hackage.haskell.org/package/aeson/docs/Data-Aeson.html
- Um tutorial mais detalhado sobre como trabalhar com JSON em Haskell: https://www.schoolofhaskell.com/school/starting-with-haskell/libraries-and-frameworks/text-manipulation/json