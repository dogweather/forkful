---
title:                "Trabalhando com yaml"
html_title:           "Elm: Trabalhando com yaml"
simple_title:         "Trabalhando com yaml"
programming_language: "Elm"
category:             "Elm"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/elm/working-with-yaml.md"
---

{{< edit_this_page >}}

## Por que usar YAML no Elm?

Você está cansado de trabalhar com arquivos de configuração complexos e difíceis de entender? Então o YAML pode ser a solução perfeita para você! Com sua sintaxe simples e legível, ele é uma ótima opção para gerenciar dados estruturados em projetos Elm.

## Como utilizar YAML no Elm

Para começar, vamos importar o pacote YAML no seu projeto Elm:

```Elm
import YAML
```

Em seguida, podemos usar a função `parse` para transformar um arquivo YAML em uma estrutura de dados Elm:

```Elm
fileContent : String
fileContent =
  """
  nome: João
  idade: 25
  profissão: Desenvolvedor
  """

parsedData : Result YAML.Error (YAML.Value)
parsedData =
  YAML.parse fileContent

case parsedData of
  Ok (YAML.Object obj) ->
    -- Faça algo com os dados do objeto aqui
  Err err ->
    -- Trate o erro aqui
```

Podemos acessar os valores do objeto de forma semelhante a um dicionário em Elm:

```Elm
case parsedData of
  Ok (YAML.Object obj) ->
    case Dict.get "nome" obj of
      Just nome ->
        -- Faça algo com o valor do nome
      Nothing ->
        -- Trate o caso em que o nome não foi encontrado
  Err err ->
    -- Trate o erro aqui
```

## Mergulhando mais fundo

Além da função `parse`, o pacote YAML também oferece outras funcionalidades úteis, como a função `encode` para transformar uma estrutura de dados Elm em YAML e as funções `merge` e `mergeDeep` para mesclar diferentes objetos YAML.

Também é possível definir tipos personalizados em YAML e convertê-los em tipos de dados Elm usando o decodificador de JSON. Isso permite que você utilize YAML para configurar a estrutura do seu aplicativo de forma mais flexível.

Não deixe de conferir a documentação completa do pacote para saber mais sobre todas as suas funcionalidades e possibilidades de uso.

## Veja também

- [Pacote YAML para Elm](https://package.elm-lang.org/packages/elm-explorations/yaml/latest/)
- [Sintaxe YAML](https://www.yaml.info/spec/)
- [Documentação oficial de Elm](https://guide.elm-lang.org/)