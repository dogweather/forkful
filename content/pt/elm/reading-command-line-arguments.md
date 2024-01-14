---
title:    "Elm: Lendo argumentos da linha de comando"
keywords: ["Elm"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pt/elm/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Por que ler argumentos da linha de comando em Elm?

Ler argumentos da linha de comando pode ser útil para diversos projetos em Elm, como a criação de ferramentas de linha de comando ou a comunicação com outras linguagens. Além disso, compreender essa funcionalidade pode expandir suas habilidades como programador Elm.

## Como fazer:

Para ler argumentos da linha de comando em Elm, vamos utilizar a função `Platform.worker`, que permite a comunicação entre o código em Elm e o JavaScript. Dentro dessa função, acessamos os argumentos através da propriedade `worker.args` e podemos manipulá-los de acordo com nossas necessidades.

```
Elm.worker
  { init = init
  , update = update
  , subscriptions = subscriptions
  , view = view
  }

port args : String -> Cmd msg
-- Porta de entrada para receber os argumentos

main : Program Never Model
main =
  Platform.worker
    { init = init
    , update = update
    , subscriptions = subscriptions
    , view = view
    , args = Decode.string
          |> Task.perform (\_ -> Error "Sem argumentos") Success
          |> Command.map String.trim
    }
```

O código acima define uma função `port`, que será utilizada como porta de entrada para receber os argumentos em formato de `String`. Em seguida, no `main`, utilizamos a função `Decode.string` para decodificar os argumentos recebidos e realizar operações com eles. Vale notar que, se nenhum argumento for passado, será retornado um resultado de erro.

## Aprofundando:

Para entender melhor o processo de leitura de argumentos da linha de comando em Elm, é importante conhecer as funções relacionadas a essa tarefa. Além da `Platform.worker`, que mencionamos anteriormente, existe também a função `Platform.programWithFlags`, que facilita o envio de argumentos durante a inicialização do programa. Outra função útil é a `Platform.command`, que permite a execução de comandos por parte do usuário. É possível encontrar mais informações sobre essas funções na documentação oficial do Elm.

## Veja também:

- Documentação oficial do Elm sobre `Platform.worker`: https://package.elm-lang.org/packages/elm/core/latest/Platform#worker
- Artigo sobre como ler argumentos da linha de comando em Elm: https://medium.com/@theabhishekpanwar/reading-command-line-arguments-in-elm-f4271748c43f
- Tutorial passo a passo para criar uma ferramenta de linha de comando em Elm: https://medium.com/@dannman/cross-platform-command-line-tool-in-elm-part-1-eb6b81012d28