---
title:                "Elm: Baixando uma página da web."
simple_title:         "Baixando uma página da web."
programming_language: "Elm"
category:             "Elm"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/elm/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Por que

Você já se perguntou como os navegadores podem exibir páginas complexas e dinâmicas? Baixar uma página da web é uma tarefa importante na programação de qualquer navegador. Nesta postagem do blog, vamos dar uma olhada em como fazer isso usando a linguagem de programação Elm.

## Como Fazer

Para baixar uma página da web em Elm, primeiro precisamos importar o módulo `Http`. Em seguida, usamos a função `get` fornecida por esse módulo, passando como argumento a URL da página que queremos baixar.

```
import Http

Http.get "https://exemplo.com" 
```

Essa função retorna uma `Task` que representa a tarefa de baixar a página. Podemos usar funções auxiliares no módulo `Task` para lidar com o resultado desse `Task` e obter o conteúdo da página.

```
handleResponse : Http.Response -> String
handleResponse response =
    case response of
        Http.BadUrl url ->
            "URL inválida: " ++ url
        Http.Timeout ->
            "Tempo limite expirado"
        Http.NetworkError ->
            "Erro de rede"
        Http.BadStatus status ->
            "Status HTTP inválido: " ++ (toString status)
        Http.GoodStatus ->
            response.body


task : Task Http.Error String
task =
    Http.get "https://exemplo.com"
        |> Task.andThen handleResponse
```

A função `handleResponse` pega a resposta HTTP e verifica se houve algum erro. Se não houver nenhum erro, ela retorna o corpo da resposta como uma `String`.

Depois de criar o `Task`, podemos executá-lo usando a função `Task.perform` e fornecendo uma função de retorno de chamada.

```
result : Effects.Effects String
result =
    task
        |> Task.perform Task.fail Task.succeed
```

A função `Task.perform` recebe duas funções como argumentos: uma para lidar com erros e outra para lidar com o sucesso. Na nossa função `handleResponse`, usamos a função `Task.succeed` para retornar o conteúdo da página como um valor de sucesso.

Por fim, podemos executar nosso programa e ver o conteúdo da página sendo impresso no console.

```
main =
    result
        |> Effects.map Debug.log "Resultado: "
```

## Aprofundando

Baixar uma página da web usando o módulo `Http` em Elm é apenas o primeiro passo. Existem muitas outras funções e métodos disponíveis neste módulo para trabalhar com solicitações HTTP. Por exemplo, podemos usar a função `Json.fromString` para converter o conteúdo da página em um valor JSON e manipulá-lo em nosso programa.

Além disso, podemos usar o módulo `Task` para lidar com tarefas assíncronas em Elm. Isso pode ser útil ao trabalhar com solicitações HTTP para páginas dinâmicas que precisam ser atualizadas regularmente.

## Veja Também

- [Documentação do módulo Http em Elm](https://package.elm-lang.org/packages/elm/http/latest/)
- [Tutorial sobre como usar o módulo Http em Elm](https://www.elm-tutorial.org/en/07-fetching-resources/01-fetching.html)