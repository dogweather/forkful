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

## O que é e porquê?
Trabalhar com JSON é uma tarefa fundamental para programadores que lidam com dados estruturados. JSON (JavaScript Object Notation) é um formato de arquivo leve e de fácil leitura, baseado em texto, que é amplamente utilizado para armazenar e transmitir dados entre diferentes sistemas. Programadores utilizam JSON para facilitar a comunicação entre aplicações e garantir que os dados sejam interpretados corretamente.

## Como fazer:
Para trabalhar com JSON em Haskell, é necessário importar o módulo "Text.JSON". Em seguida, é possível utilizar funções como "readJSON" para ler um arquivo JSON e convertê-lo para um tipo de dado Haskell, e "encode" para transformar um tipo de dado Haskell para JSON. Veja um exemplo abaixo:

```Haskell
import Text.JSON
main :: IO ()
main = do
  let jsonString = "{\"nome\": \"João\", \"idade\": 25}" -- Exemplo de string JSON
  let result = decode jsonString :: Result JSValue -- Converte o JSON para o tipo JSValue
  case result of
    Ok value -> putStrLn (encode value) -- Converte o JSValue de volta para JSON e imprime na tela
    Error msg -> print msg -- Em caso de erro, imprime a mensagem de erro
```

Resultado:
```
{"nome":"João","idade":25}
```

## Mais detalhes:
JSON foi criado em 1999 por Douglas Crockford e atualmente é um dos formatos de dados mais populares na web devido à sua simplicidade e interoperabilidade. Além do Haskell, outros linguagens de programação, como JavaScript e Python, também oferecem suporte nativo a JSON. Existem ainda bibliotecas externas em Haskell, como "Aeson" e "JSON2", que oferecem funcionalidades mais avançadas para trabalhar com JSON.

## Veja também:
- [Documentação oficial do módulo "Text.JSON" em Haskell](https://hackage.haskell.org/package/json)
- [Mais informações sobre o formato JSON](https://www.json.org/json-pt.html)
- [Outras bibliotecas em Haskell para trabalhar com JSON](https://hackage.haskell.org/packages/search?terms=json)