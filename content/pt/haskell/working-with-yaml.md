---
title:                "Haskell: Trabalhando com yaml"
simple_title:         "Trabalhando com yaml"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/haskell/working-with-yaml.md"
---

{{< edit_this_page >}}

## Por que trabalhar com YAML é importante para programadores Haskell?

Haskell é uma linguagem de programação funcional pura e altamente tipada, o que significa que trabalha com tipos de dados de forma rigorosa e precisa. YAML, por sua vez, é uma linguagem de serialização de dados que torna mais fácil a tarefa de armazenar e transportar dados entre diferentes sistemas e plataformas.

Juntas, Haskell e YAML oferecem uma poderosa combinação para programadores, permitindo uma fácil manipulação de dados e configurações complexas. Além disso, YAML é simples de ler e escrever, o que torna mais fácil para diferentes membros de uma equipe de desenvolvimento trabalharem juntos em um mesmo projeto.

## Como trabalhar com YAML em Haskell?

A biblioteca [Yaml](https://hackage.haskell.org/package/yaml) oferece uma interface completa para trabalhar com YAML em Haskell. Começando pela instalação da biblioteca em seu projeto, podemos então importá-la e começar a utilizá-la em nossos códigos:

```Haskell
import Data.Yaml

main = do
  -- Lendo um arquivo YAML e retornando seus valores
  config <- decodeFileThrow "config.yaml" :: IO (Maybe Value)
  
  -- Imprimindo o conteúdo do arquivo
  print config
```

No exemplo acima, utilizamos a função `decodeFileThrow` para ler o arquivo `config.yaml` e retornar seus valores. Utilizamos também a anotação de tipo `:: IO (Maybe Value)` para informar que o retorno é do tipo `IO`, indicando que é uma ação do Monoid Async, e `Maybe Value`, indicando que o valor retornado pode ser `Just` (valor existente) ou `Nothing` (valor não existente).

Podemos então utilizar as funções da biblioteca para acessar e manipular as informações do arquivo YAML, de acordo com a estrutura desejada.

## Mergulhando mais fundo em YAML e Haskell

Além de ler e escrever arquivos YAML, a biblioteca Yaml também oferece funções para tratar e validar os dados dentro do arquivo, como por exemplo a função `checkKey` para verificar se uma chave existe no arquivo.

A linguagem Haskell também oferece recursos poderosos, como a validação de tipos de dados em tempo de compilação, que podem ser utilizados em conjunto com o YAML para garantir a integridade dos dados em seu código.

Para saber mais sobre como trabalhar com YAML em Haskell, recomendamos a leitura do tutorial oficial [Yaml Configuration Files](https://www.schoolofhaskell.com/user/commercial/content/yaml-serialization-configuration-files) e a exploração da documentação da biblioteca [Yaml](https://hackage.haskell.org/package/yaml).

## Veja também

- [Tutorial Yaml Configuration Files](https://www.schoolofhaskell.com/user/commercial/content/yaml-serialization-configuration-files)
- [Documentação Yaml Haskell](https://hackage.haskell.org/package/yaml)