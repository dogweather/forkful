---
title:                "Elm: Obtendo a data atual"
programming_language: "Elm"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/elm/getting-the-current-date.md"
---

{{< edit_this_page >}}

##Por que

Muitas vezes, quando estamos desenvolvendo um programa, precisamos saber a data atual. Isso pode ser útil para rastrear eventos, gerar relatórios ou até mesmo simplesmente exibir a data para o usuário. Em Elm, há uma maneira fácil de obter a data atual e é isso que vamos aprender neste post.

##Como fazer

Para obter a data atual em Elm, podemos usar a função `Time.now`, que retorna uma `Task` que, quando concluída, nos fornece um `Time` com a data atual. Aqui está um exemplo de código:

```
Elm.Program
Elm.Time
import Time exposing (Time)

main : Program Never
main =
    let
        getDate : Time -> String
        getDate time =
            Time.toIsoString Time.utc time
    in
    Time.now
        |> Task.attempts 1
        |> Task.andThen (\result ->
            case result of
                Ok time ->
                    getDate time |> Html.text

                Err err ->
                    "Não foi possível obter a data atual." |> Html.text
        )
        |> Html.program
```

A primeira coisa que fazemos é importar o módulo `Time` do pacote `elm/time`. Em seguida, definimos a função `getDate` que irá converter o objeto `Time` em uma string no formato ISO. Isso será útil para exibir a data em um formato legível para o usuário.

Em seguida, usamos a função `Time.now` para obter a data atual como uma `Task`, que representa uma computação assíncrona que será concluída em algum momento e nos fornecerá um valor. Usamos a função `Task.attempts` para especificar o número máximo de tentativas que o Elm deve fazer para executar a tarefa. No nosso caso, queremos apenas uma tentativa. Depois, usamos a função `Task.andThen` para receber o resultado da tarefa e fazer algo com ele. Se a tarefa for bem-sucedida, obtemos um objeto `Time` e usamos nossa função `getDate` para convertê-lo em uma string. Se a tarefa falhar, exibimos uma mensagem de erro.

Por último, usamos a função `Html.program` para criar um programa HTML que exibe a data atual. E é isso!

##Mergulho profundo

Mas, como o Elm obtém a data atual? Como sabemos que é a data atual quando recebemos o objeto `Time` da tarefa? Bem, o segredo está no módulo `Time`, que usa JavaScript nativo para obter a data atual do sistema operacional do usuário. Portanto, a data exibida depende da data e hora definidas no sistema operacional.

##Veja também

- [Documentação do módulo `Time`](https://package.elm-lang.org/packages/elm/time/latest/Time)
- [Tutorial básico de Elm](https://www.elm-tutorial.org/pt-br/)